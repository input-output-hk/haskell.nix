# Generate cache entries for dependencies of package defined in `src`

{ pkgs, haskellLib }:
{ src
, stackYaml    ? "stack.yaml"
, sha256map    ? null
                     # An alternative to adding `# nix-sha256:` comments into the
                     # stack.yaml file:
                     #   sha256map =
                     #     { "https://github.com/jgm/pandoc-citeproc"."0.17"
                     #         = "0dxx8cp2xndpw3jwiawch2dkrkp15mil7pyx7dvd810pwc22pm2q"; };
, lookupSha256 ?
  if sha256map != null
    then { location, tag, ...}: sha256map."${location}"."${tag}"
    else _: null
, branchMap    ? null
                     # A way to specify in which branch a git commit can
                     # be found
, lookupBranch ?
  if branchMap != null
    then { location, tag, ...}: branchMap."${location}"."${tag}" or null
    else _: null
, resolverSha256 ? null
, nix-tools ? pkgs.haskell-nix.internal-nix-tools # When building stack projects we use the internal nix-tools (compiled with a fixed GHC version)
, ...
}:
let
    # We only care about the stackYaml file.  If src is a local directory
    # we want to avoid recalculating the cache unless the stack.yaml file
    # changes.

    inherit (haskellLib.fetchResolver {
        inherit src stackYaml resolverSha256;
      }) resolver fetchedResolver;

    # Filter just the stack yaml file and any resolver yaml file it points to.
    maybeCleanedSource =
      if haskellLib.canCleanSource src
        then haskellLib.cleanSourceWith {
          inherit src;
          filter = path: type:
            let
              origSrc = if src ? _isLibCleanSourceWith then src.origSrc else src;
              origSubDir = if src ? _isLibCleanSourceWithEx then src.origSubDir else "";
              relPath = pkgs.lib.removePrefix (toString origSrc + origSubDir + "/") path;

              # checks if path1 is a parent directory for path2
              isParent = path1: path2: pkgs.lib.hasPrefix "${path1}/" path2;

            in
              (relPath == stackYaml)
              || (resolver != null && (relPath == resolver || isParent relPath resolver))
            ;
        }
        else src;

    # All repos served via ssh or git protocols are usually private
    private = url: pkgs.lib.substring 0 4 url != "http";

    repos = builtins.fromJSON (builtins.readFile (pkgs.runCommand "stack-repos" {
        buildInputs = [ nix-tools ];
      } ''
        TMP=$(mktemp -d)
        cd $TMP
        cp -r "${maybeCleanedSource}/." $TMP
        chmod -R +w $TMP
        substituteInPlace ${stackYaml} --replace "# nix-sha256:" "nix-sha256:"
        ${pkgs.lib.optionalString (fetchedResolver != null) ''
          substituteInPlace ${stackYaml} --replace "${resolver}" "${fetchedResolver}"
        ''}
        stack-repos --stack-yaml ${stackYaml}
        cp repos.json $out
      ''));

    cabalName = path: builtins.readFile (pkgs.runCommand "cabal-name" {
        buildInputs = [ nix-tools ];
      } ''
        cabal-name ${path} > $out
      '');

    hashPath = path:
        builtins.readFile (pkgs.runCommand "hash-path" { preferLocalBuild = true; }
            "echo -n $(${pkgs.nix}/bin/nix-hash --type sha256 --base32 ${path}) > $out");
in with pkgs.lib;
concatMap (dep:
        let
            is-private = private dep.url;
            sha256 = if dep.sha256 != null
              then dep.sha256
              else lookupSha256 {
                  location = dep.url;
                  tag = dep.rev;
                };
            branch = lookupBranch {
              location = dep.url;
              tag = dep.rev;
            };
            pkgsrc =
              if !is-private && sha256 != null
                then pkgs.fetchgit {
                  inherit (dep) url rev;
                  inherit sha256;
                }
                else builtins.fetchGit ({
                  inherit (dep) url rev;
                } // (if branch != null
                  then { ref = branch; }
                  # Don't fail if rev not in default branch:
                  else { allRefs = true; })
                );
        in map (subdir: {
                name = cabalName "${pkgsrc}/${subdir}";
                inherit (dep) url rev;
                inherit is-private;
                sha256 = if !is-private then hashPath pkgsrc else null;
            } // (optionalAttrs (subdir != "") { inherit subdir; }))
        (dep.subdirs or [ "" ])) repos

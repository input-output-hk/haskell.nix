# Generate cache entries for dependencies of package defined in `src`

{ pkgs, haskellLib, nix-tools }:
{ src, stackYaml ? "stack.yaml" }:
let
    # We only care about the stackYaml file.  If src is a local directory
    # we want to avoid recalculating the cache unless the stack.yaml file
    # changes.

    # Using origSrcSubDir bypasses any cleanSourceWith so that it will work when
    # access to the store is restricted.  If origSrc was already in the store
    # you can pass the project in as a string.
    rawStackYaml = builtins.readFile ((src.origSrcSubDir or src) + ("/" + stackYaml));

    # Determine the resolver as it may point to another file we need
    # to look at.
    resolver =
      let
        rs = pkgs.lib.lists.concatLists (
          pkgs.lib.lists.filter (l: l != null)
            (builtins.map (l: builtins.match "^resolver: *(.*)" l)
              (pkgs.lib.splitString "\n" rawStackYaml)));
      in
        pkgs.lib.lists.head (rs ++ [ null ]);

    # Filter just the stack yaml file and any reolver yaml file it points to.
    maybeCleanedSource =
      if haskellLib.canCleanSource src
        then haskellLib.cleanSourceWith {
          inherit src;
          filter = path: type:
               pkgs.lib.hasSuffix ("/" + stackYaml) path
            || (resolver != null && pkgs.lib.hasSuffix ("/" + resolver) path);
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
            pkgsrc =
              if !is-private && dep.sha256 != null
                then pkgs.fetchgit {
                  inherit (dep) url rev sha256;
                }
                else builtins.fetchGit {
                  inherit (dep) url rev;
                  ref = "*";
                };
        in map (subdir: {
                name = cabalName "${pkgsrc}/${subdir}";
                inherit (dep) url rev;
                inherit is-private;
                sha256 = if !is-private then hashPath pkgsrc else null;
            } // (optionalAttrs (subdir != "") { inherit subdir; }))
        (dep.subdirs or [ "" ])) repos

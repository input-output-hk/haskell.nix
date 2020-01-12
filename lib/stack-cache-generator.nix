# Generate cache entries for dependencies of package defined in `src`

{ pkgs, haskellLib, nix-tools }:
{ src, stackYaml ? "stack.yaml" }:
let
    # We only care about the stackYaml file.  If src is a local directory
    # we want to avoid recalculating the cache unless the stack.yaml file
    # changes.
    maybeCleanedSource =
      if haskellLib.canCleanSource src
        then haskellLib.cleanSourceWith {
          inherit src;
          filter = path: type: pkgs.lib.hasSuffix ("/" + stackYaml) path;
        }
        else src;

    # All repos served via ssh or git protocols are usually private
    private = url: pkgs.lib.substring 0 4 url != "http";
    
    repos = builtins.fromJSON (builtins.readFile (pkgs.runCommand "stack-repos" {
        buildInputs = [ nix-tools ];
      } ''
        TMP=$(mktemp -d)
        cd $TMP
        cp "${maybeCleanedSource}/${stackYaml}" stack.yaml
        substituteInPlace stack.yaml --replace "# nix-sha256:" "nix-sha256:"
        stack-repos
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

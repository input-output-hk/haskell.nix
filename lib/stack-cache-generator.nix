# Generate cache entries for dependencies of package defined in `src`

{ pkgs }:
{ src, stackYaml ? "stack.yaml" }:
let
    s2n = import ../pkgs/stack-to-nix pkgs;

    # All repos served via ssh or git protocols are usually private
    private = url: pkgs.lib.substring 0 4 url != "http";
    
    deps = (s2n.importYAML "${src}/${stackYaml}").extra-deps or [ ];
    hashPath = path:
        builtins.readFile (pkgs.runCommand "hash-path" { preferLocalBuild = true; }
            "echo -n $(${pkgs.nix}/bin/nix-hash --type sha256 --base32 ${path}) > $out");
in with pkgs.lib;
concatMap (dep:
    if !builtins.isAttrs dep then
        [ ]
    else
        let
            pkgsrc = builtins.fetchGit {
                url = dep.git;
                ref = "*";
                rev = dep.commit;
            };
        in map (subdir:
            rec {
                name = s2n.cabalPackageName "${pkgsrc}/${subdir}";
                rev = dep.commit;
                url = dep.git;
                is-private = private url;
                sha256 = if !is-private then hashPath pkgsrc else null;
            } // (optionalAttrs (subdir != "") { inherit subdir; }))
        (dep.subdirs or [ "" ])) deps

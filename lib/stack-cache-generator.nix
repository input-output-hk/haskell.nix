# Generate cache entries for dependencies of package defined in `src`

{ pkgs }:
{ src, stackYaml ? "stack.yaml" }:
let
    s2n = import "${pkgs.fetchFromGitHub {
       owner = "serokell";
       repo = "stack-to-nix";
       rev = "28e690d3eddd47c59982c7fbf4f950320ff7ff69";
       sha256 = "1xnx5baj3k29iy8ccppn28ayf4483zddrvq6fikfpvblzp5zrnaj";
    }}/lib.nix" pkgs;

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
            {
                name = s2n.cabalPackageName "${pkgsrc}/${subdir}";
                rev = dep.commit;
                url = dep.git;
                sha256 = hashPath pkgsrc;
                is-private = private dep.git;
            } // (optionalAttrs (subdir != "") { inherit subdir; }))
        (dep.subdirs or [ "" ])) deps

{
  lib
, haskellLib
}: {
  devshell = final: prev: {
    devshell = let
      cabal-install = final.pkgs.buildPackages.haskell-nix.cabal-install.${final.args.compiler-nix-name};
      inherit (final.pkgs.buildPackages) runCommand runtimeShell;
      cabalWrapper = runCommand "cabal" { inherit (cabal-install) meta; } ''
        mkdir -p $out/bin
        cat << EOF > $out/bin/cabal
        #!${runtimeShell}
        set -euo pipefail

        find_up() {
          while [[ \$PWD != / ]] ; do
            if [[ -e "\$1" ]]; then
              echo "\$PWD"
              return
            fi
            cd ..
          done
        }

        toplevel=\$(find_up "cabal.project")

        if [[ -n "\$toplevel" ]]; then
          cabal_project="\$toplevel/cabal.project"
          nix_cabal_project=\$toplevel/.nix-cabal.project
          extra_cabal_opts=("--project-file=\$nix_cabal_project")
          awk '
            # Add comment with explanation of file
            BEGIN {
              print "-- Generated from '"\$cabal_project"' by the wrapper script"
              print "-- ${placeholder "out"}/cabal"
              print "-- Add this file to your .gitignore\n"
            }

            # Matches all section starts
            /^[^ ]/ {
              # Remember the section name (they can span multiple lines)
              section = \$0
            }
            # Matches every line
            // {
              # Only print the line if it is not in the section we want to omit
              if (section != "source-repository-package")
                print \$0
            }
          ' "\$cabal_project" > "\$nix_cabal_project"
        else
          extra_cabal_opts=()
        fi

        cabal=${placeholder "out"}/bin/.cabal
        >&2 echo "\$cabal \''${extra_cabal_opts[@]} \$@"
        exec "\$cabal" "\''${extra_cabal_opts[@]}" "\$@"
        EOF
        cp -rn ${cabal-install}/* $out/
        cp ${cabal-install}/bin/cabal $out/bin/.cabal
        chmod +x $out/bin/*
      '';
    in {
      packages = final.shell.nativeBuildInputs;
      env = lib.mapAttrsToList lib.nameValuePair {
        inherit (final.shell) CABAL_CONFIG NIX_GHC_LIBDIR;
      };
      commands = [
        {
          package = cabalWrapper;
          category = "development";
        }
      ];
    };
  };

  projectComponents = final: prev: let
    packages = haskellLib.selectProjectPackages final.hsPkgs;
    # used to materialize `packages-exes.nix` (project packages and exes list) to avoid some evaluations:
    packagesExes = lib.genAttrs
      (lib.attrNames final.packages)
      (name: lib.attrNames final.hsPkgs.${name}.components.exes);
    collectExes = packagesExes: lib.listToAttrs (lib.concatLists (lib.mapAttrsToList
      (p: map (exe: lib.nameValuePair exe final.hsPkgs.${p}.components.exes.${exe})) packagesExes));
    in {
      # local project packages:
      packages = haskellLib.selectProjectPackages final.hsPkgs;
      # set of all exes (as first level entries):
      exes = collectExes packagesExes;
      # set of all exes (as first level entries), mapped from a materialized set of packages and exes set (generated via `generatePackagesExes`):
      exesFrom = packagesExesMat: collectExes (import packagesExesMat);
      # `tests` are the test suites which have been built.
      tests = haskellLib.collectComponents' "tests" final.packages;
      # `benchmarks` (only built, not run).
      benchmarks = haskellLib.collectComponents' "benchmarks" final.packages;
      # `checks` collect results of executing the tests:
      checks = haskellLib.collectChecks' final.packages;

      generatePackagesExesMat = let
        nixFile = builtins.toFile "packages-exes.nix" (lib.generators.toPretty {} packagesExes);
      in final.pkgs.buildPackages.writeShellScript "generate-packages-exes" ''
          set -euo pipefail

          TARGET="$1"
          TARGET_DIR="$(dirname "$TARGET")"

          mkdir -p "$TARGET_DIR"
          rm -rf "$TARGET"
          cp -r ${nixFile} "$TARGET"
          chmod -R +w "$TARGET"
      '';
  };

}

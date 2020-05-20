# Update script to update nix-tools.
#
# This scipt might still come in handy, but it should now be possible to update
# the nix-tools materialization without it.
#
# Without this script:
# 1. run `nix-prefetch-git https://github.com/input-output-hk/nix-tools > nix-tools-src-new.json`
# 2. run `nix-build -E '(import ./. { checkMaterialization = true; }).pkgs.haskell-nix.nix-tools'`
# 3. If it fails apply the fixes from the log (there should be rm, cp and chmod commands)
#
# To bootstap materialization files for a new compiler (e.g. for ghc 8.8.3)
#
# nix-build -E 'let h = (import ./. {}).pkgs.haskell-nix; in h.cabal-install-tool { compiler-nix-name = "ghc883"; checkMaterialization = true; inherit (h) cabal-install nix-tools; }'
# nix-build -E 'let h = (import ./. {}).pkgs.haskell-nix; in h.nix-tools-set { compiler-nix-name = "ghc883"; checkMaterialization = true; inherit (h) cabal-install nix-tools; }'
# nix-build -E 'let h = (import ./. {}).pkgs.haskell-nix; in h.alex-tool { compiler-nix-name = "ghc883"; checkMaterialization = true; inherit (h) cabal-install nix-tools; }'
# nix-build -E 'let h = (import ./. {}).pkgs.haskell-nix; in h.happy-tool { compiler-nix-name = "ghc883"; checkMaterialization = true; inherit (h) cabal-install nix-tools; }'
#
# Using the script:
# 1. run `nix-prefetch-git https://github.com/input-output-hk/nix-tools > nix-tools-src-new.json`
# 2. run `nix-build regenerate.nix --show-trace --arg specJSON ./nix-tools-src-new.json`
# 3. run `./result/bin/update-nix-tools` and follow the instructions.
# 4. git commit -a -m "bump nix-tools"
#
let
  inherit (import ../default.nix {}) sources nixpkgsArgs;
in
{ specJSON ? ./nix-tools-src.json }:
with import sources.nixpkgs-default nixpkgsArgs;
let
  src = haskell-nix.cleanSourceHaskell {
    src = haskell-nix.fetchExternal {
      name     = "nix-tools-src";
      inherit specJSON;
      override = "nix-tools-src";
    };
  };
in
  let plan-to-nix = (haskell-nix.cabalProject {
    name = "nix-tools";
    inherit src;
    modules = [{
     # Remove Cabal from nonReinstallablePkgs to be able to pick Cabal-3.2.
     nonReinstallablePkgs= [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
      "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
      # ghcjs custom packages
      "ghcjs-prim" "ghcjs-th"
      "ghc-boot"
      "ghc" "Win32" "array" "binary" "bytestring" "containers"
      "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
      # "ghci" "haskeline"
      "hpc"
      "mtl" "parsec" "process" "text" "time" "transformers"
      "unix" "xhtml"
      # "stm" "terminfo"
     ];
    }];
  }).nix-tools.components.exes.plan-to-nix; in
with builtins;
with stdenv.lib;
writeShellScriptBin "update-nix-tools" ''
   export PATH="${getBin nix}/bin:${getBin nix-prefetch-scripts}/bin:${getBin git}/bin:${getBin haskell-nix.cabal-install}/bin:${getBin plan-to-nix}/bin:${getBin coreutils}/bin:${getBin findutils}/bin"

   TMP=$(mktemp -d)
   echo $TMP

   cd $TMP
   cp ${src}/nix-tools.cabal .
   cp ${src}/cabal.project .
   cp ${specJSON} ./nix-tools-src.json

   echo "--> Updating cabal index..."
   cabal v2-update -v0

   # build for the current ghc in haskell.nix
   echo "--> Configuring nix-tools for ${haskell-nix.ghc.name}..."
   cabal v2-configure -w ${getBin haskell-nix.ghc}/bin/ghc -v0
   echo "--> Running plan-to-nix for ${haskell-nix.ghc.name}..."
   plan-to-nix -o . --plan-json=$(find . -name "plan.json")

   rm cabal.project.local
   rm -fR dist-newstyle

   # we don't want the default.nix that plan-to-nix generates to
   # update the custom one in nix-tools
   mv pkgs.nix default.nix

   rm -f nix-tools.cabal
   echo "--> Done."
   echo "******"
   echo "*** please copy $TMP/ into haskell.nix/materialized/GHCVER/nix-tools"
   echo "***"
   echo "***   cp -fr "$TMP/" ../materialized/GHCVER/nix-tools/"
   echo "***"
   echo "******"
''

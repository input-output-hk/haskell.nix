#
# Update script to update nix-tools.
#
# 1. run `nix-prefetch-git https://github.com/input-output-hk/nix-tools > nix-tools-src-new.json`
# 2. run `nix-build regenerate.nix --show-trace --arg specJSON ./nix-tools-src-new.json`
# 3. run `./result/bin/update-nix-tools` and follow the instructions.
# 4. git commit -a -m "bump nix-tools"
#
{ specJSON ? ./nix-tools-src.json }:
with import (import ../nixpkgs).nixpkgs-default (import ../default.nix).nixpkgsArgs;
let
  src = haskell-nix.cleanSourceHaskell {
    src = haskell-nix.fetchExternal {
      name     = "nix-tools-src";
      inherit specJSON;
      override = "nix-tools-src";
    };
  };
in
let plan-to-nix = (haskell-nix.cabalProject { name = "nix-tools"; inherit src; }).nix-tools.components.exes.plan-to-nix; in
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

   # Build for ghc-8.4.4
   echo "--> Updating cabal index..."
   cabal v2-update -v0
   echo "--> Configuring nix-tools for ${haskell-nix.compiler.ghc844.name}..."
   cabal v2-configure -w ${getBin haskell-nix.compiler.ghc844}/bin/ghc -v0
   echo "--> Running plan-to-nix for ${haskell-nix.compiler.ghc844.name}..."
   plan-to-nix -o . --plan-json=$(find . -name "plan.json")

   rm cabal.project.local
   rm -fR dist-newstyle

   mv pkgs.nix pkgs-8.4.4.nix

   # build for the current ghc in haskell.nix
   echo "--> Configuring nix-tools for ${haskell-nix.ghc.name}..."
   cabal v2-configure -w ${getBin haskell-nix.ghc}/bin/ghc -v0
   echo "--> Running plan-to-nix for ${haskell-nix.ghc.name}..."
   plan-to-nix -o . --plan-json=$(find . -name "plan.json")

   rm cabal.project.local
   rm -fR dist-newstyle

   # we don't want the default.nix that plan-to-nix generates to
   # update the custom one in nix-tools
   rm default.nix

   rm -f nix-tools.cabal
   echo "--> Done."
   echo "******"
   echo "*** please copy $TMP/* into haskell.nix/nix-tools"
   echo "***"
   echo "***   cp -fr "$TMP/" ."
   echo "***"
   echo "******"
''

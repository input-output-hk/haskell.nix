#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash jq nix

set -euo pipefail

NIX_BUILD_ARGS="${NIX_BUILD_ARGS:-}"

cd $(dirname $0)

printf "*** Cleaning package build directories..." >& 2
rm -rvf */cabal.project.local */.ghc.environment* */dist */dist-newstyle */.stack-work
echo >& 2

printf "*** Running the nix-build tests...\n" >& 2
nix build $NIX_BUILD_ARGS --no-link --keep-going -f ./default.nix
echo >& 2

printf "*** Running the unit tests... " >& 2
res=$(nix-instantiate --eval --json --strict ./default.nix -A unit)
num_failed=$(jq length <<< "$res")
if [ $num_failed -eq 0 ]; then
  printf "PASSED\n" >& 2
else
  printf "$num_failed FAILED\n" >& 2
  jq . <<< "$res"
  exit 1
fi

printf "*** Checking that a nix-shell works for runghc...\n" >& 2
nix-shell $NIX_BUILD_ARGS \
    --pure ./default.nix \
    -A with-packages.test-shell \
    --run 'runghc with-packages/Point.hs'
echo >& 2

printf "*** Checking that a nix-shell works for cabal...\n" >& 2
nix-shell $NIX_BUILD_ARGS \
    --pure ./default.nix \
    -A with-packages.test-shell \
    --run 'echo CABAL_CONFIG=$CABAL_CONFIG && type -p ghc && cd with-packages && cabal new-build'
echo >& 2

printf "*** Checking that a nix-shell works for cabal (doExactConfig component)...\n" >& 2
nix-shell $NIX_BUILD_ARGS \
    --pure ./default.nix \
    -A with-packages.test-shell-dec \
    --run 'echo CABAL_CONFIG=$CABAL_CONFIG && echo GHC_ENVIRONMENT=$GHC_ENVIRONMENT && cd with-packages && cabal new-build'
echo >& 2

printf "*** Checking that a nix-shell works for a multi-target project...\n" >& 2
nix-shell $NIX_BUILD_ARGS \
    --pure ./default.nix \
    -A cabal-simple.test-shell \
    --run 'cd cabal-simple && cabal new-build'
echo >& 2

printf "*** Checking shellFor works for a cabal project, multiple packages...\n" >& 2
nix-shell $NIX_BUILD_ARGS \
    --pure ./default.nix \
    -A shell-for.env \
    --run 'cd shell-for && cabal new-build all'
echo >& 2

printf "*** Checking shellFor works for a cabal project, single package...\n" >& 2
nix-shell $NIX_BUILD_ARGS \
    --pure ./default.nix \
    -A shell-for.envPkga \
    --run 'cd shell-for && cabal new-build pkga'
echo >& 2

printf "*** Checking shellFor has a working hoogle index...\n" >& 2
nix-shell $NIX_BUILD_ARGS \
    --pure ./default.nix \
    -A shell-for.env \
    --run 'hoogle ConduitT | grep Data.Conduit'
echo >& 2

printf "\n*** Finished successfully\n" >& 2

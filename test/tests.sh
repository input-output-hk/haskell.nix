#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash jq nix gnused

set -euo pipefail

NIX_BUILD_ARGS="${NIX_BUILD_ARGS:-}"

cd $(dirname $0)

if [ "$#" != "1" ]; then
  echo "Please pass a compiler-nix-name to use.  For example: ./test/test.sh ghc884"
  exit 1
fi

GHC=$1

printf "*** Cleaning package build directories..." >& 2
rm -rvf */cabal.project.local */.ghc.environment* */dist */dist-newstyle */.stack-work
echo >& 2

printf "*** Running the nix-build tests...\n" >& 2
nix build $NIX_BUILD_ARGS \
   -I . -I .. \
   --option restrict-eval true \
   --option allowed-uris "https://github.com/NixOS https://github.com/input-output-hk https://github.com/edolstra/flake-compat https://api.github.com" \
   --no-link --keep-going -f default.nix \
   --argstr compiler-nix-name $GHC
echo >& 2

printf "*** Running the unit tests... " >& 2
res=$(nix-instantiate --eval --json --strict ./default.nix --argstr compiler-nix-name $GHC -A unit.tests)
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
    --argstr compiler-nix-name $GHC \
    -A with-packages.test-shell \
    --run 'runghc with-packages/Point.hs'
echo >& 2

printf "*** Checking that a nix-shell works for cabal...\n" >& 2
nix-shell $NIX_BUILD_ARGS \
    --pure ./default.nix \
    --argstr compiler-nix-name $GHC \
    -A with-packages.test-shell \
    --run 'echo CABAL_CONFIG=$CABAL_CONFIG && type -p ghc && cd with-packages && cabal new-build'
echo >& 2

printf "*** Checking that a nix-shell works for cabal (doExactConfig component)...\n" >& 2
nix-shell $NIX_BUILD_ARGS \
    --pure ./default.nix \
    --argstr compiler-nix-name $GHC \
    -A with-packages.test-shell-dec \
    --run 'echo CABAL_CONFIG=$CABAL_CONFIG && echo GHC_ENVIRONMENT=$GHC_ENVIRONMENT && cd with-packages && cabal new-build'
echo >& 2

printf "*** Checking that a nix-shell works for a project with test-suite build-tools and benchmarks...\n" >& 2
printf "!!! This is expected to fail until https://github.com/input-output-hk/haskell.nix/issues/231 is resolved! \n" >& 2
nix-shell $NIX_BUILD_ARGS \
    --pure ./default.nix \
    --argstr compiler-nix-name $GHC \
    -A cabal-22.shell \
    --run 'cd cabal-22 && cabal new-build all --enable-tests --enable-benchmarks' \
    || true
echo >& 2

printf "*** Checking that a nix-shell works for a multi-target project...\n" >& 2
nix-shell $NIX_BUILD_ARGS \
    --pure ./default.nix \
    --argstr compiler-nix-name $GHC \
    -A cabal-simple.test-shell \
    --run 'cd cabal-simple && cabal new-build'
echo >& 2

printf "*** Checking shellFor works for a cabal project, multiple packages...\n" >& 2
nix-shell $NIX_BUILD_ARGS \
    --pure ./default.nix \
    --argstr compiler-nix-name $GHC \
    -A shell-for.env \
    --run 'cd shell-for && cabal new-build all'
echo >& 2

printf "*** Checking shellFor works for a cabal project, single package...\n" >& 2
nix-shell $NIX_BUILD_ARGS \
    --pure ./default.nix \
    --argstr compiler-nix-name $GHC \
    -A shell-for.envPkga \
    --run 'cd shell-for && cabal new-build --project=single.project all'
echo >& 2

printf "*** Checking shellFor has a working hoogle index...\n" >& 2
nix-shell $NIX_BUILD_ARGS \
    --pure ./default.nix \
    --argstr compiler-nix-name $GHC \
    -A shell-for.env \
    --run 'hoogle ConduitT | grep Data.Conduit'
echo >& 2

printf "*** Checking shellFor does not depend on given packages...\n" >& 2
drva=$(nix-instantiate ./default.nix --argstr compiler-nix-name $GHC -A shell-for.env)
echo "-- hello" >> shell-for/pkga/PkgA.hs
drvb=$(nix-instantiate ./default.nix --argstr compiler-nix-name $GHC -A shell-for.env)
sed -i -e '/-- hello/d' shell-for/pkga/PkgA.hs
if [ "$drva" != "$drvb" ]; then
    printf "FAIL\nShell derivations\n$drva\n$drvb\n are not identical.\n" >& 2
    exit 1
else
    printf "PASS\n" >& 2
fi

printf "*** Checking the maintainer scripts...\n" >& 2
nix build $NIX_BUILD_ARGS --no-link --keep-going -f ../build.nix --argstr compiler-nix-name $GHC maintainer-scripts
echo >& 2

printf "*** Checking that plan construction works with extra Hackages...\n" >& 2
nix build $NIX_BUILD_ARGS --no-link \
    -f ./default.nix --argstr compiler-nix-name $GHC \
    extra-hackage.run.project.plan-nix
echo >& 2

printf "*** Checking that package with extra Hackages can be build...\n" >& 2
nix build $NIX_BUILD_ARGS --no-link \
    -f ./default.nix --argstr compiler-nix-name $GHC \
    extra-hackage.run.project.hsPkgs.external-package-user.components.exes.external-package-user
echo >& 2

printf "\n*** Finished successfully\n" >& 2

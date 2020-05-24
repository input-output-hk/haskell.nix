# { stdenv, lib, haskellLib, srcOnly, cabal-install }:
{ cabal-install
, runCommand
, name
, src
}:

runCommand ("${name}-cabal-check") {
    nativeBuildInputs = [ cabal-install ];
} ''
  cp -r ${src}/* ./
  touch "$out"

  runHook preBuild

  cabal check | tee "$out"

  runHook postBuild
''
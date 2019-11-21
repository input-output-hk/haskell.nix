{ stdenv, lib, haskellLib }:
drv:

let
  component = drv.config;

# This derivation can be used to execute test component.
# The $out of the derivation is a file containing the resulting
# stdout output.
in stdenv.mkDerivation ({
  name = (drv.name + "-run");

  src = drv;

  passthru = {
    inherit (drv) identifier config configFiles executableToolDepends cleanSrc env;
  };

  inherit (drv) meta LANG LC_ALL;

  inherit (component) doCheck doCrossCheck;

  phases = ["checkPhase"];

  checkPhase = ''
    runHook preCheck

    ${toString component.testWrapper} $src/${drv.installedExe} ${lib.concatStringsSep " " component.testFlags} | tee $out

    runHook postCheck
  '';
} // haskellLib.optionalHooks {
  inherit (component) preCheck postCheck;
})

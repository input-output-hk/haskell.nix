{ stdenv, lib, haskellLib }:
drv:

let
  component = drv.config;

# This derivation can be used to execute test component.
# The $out of the derivation is a file containing the resulting
# stdout output.
in stdenv.mkDerivation ({
  name = (drv.name + "-check");

  src = drv;

  passthru = {
    inherit (drv) identifier config configFiles executableToolDepends cleanSrc env;
  };

  inherit (drv) meta LANG LC_ALL;

  inherit (component) doCheck doCrossCheck;

  phases = ["buildPhase" "checkPhase"];

  # If doCheck or doCrossCheck are false we may still build this
  # component and we want it to quietly succeed.
  buildPhase = ''
    touch $out
  '';

  checkPhase = ''
    runHook preCheck

    ${toString component.testWrapper} $src/${drv.installedExe} ${lib.concatStringsSep " " component.testFlags} | tee $out

    runHook postCheck
  '';
} // haskellLib.optionalHooks {
  inherit (component) preCheck postCheck;
})

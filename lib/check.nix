{ stdenv, lib, haskellLib }:
drv:

let
  component = drv.config;

# This derivation can be used to execute test component.
# The $out of the derivation is a file containing the resulting
# stdout output.
in stdenv.mkDerivation ((
  if drv ? source
    then {
      src = drv.source;
      patchPhase =
        # This `cd` is normally done in the `prePatch` of the drv
        lib.optionalString (drv.srcSubDir != "") ''
          cd ${lib.removePrefix "/" drv.srcSubDir}
        '';
    }
    else
      # This makes the derivation work a bit like `srcOnly`,
      # using the original derivation, but replacing the `buildPhase`.
      (drv.drvAttrs or drv) // {
        outputs = [ "out" ];
        separateDebugInfo = false;
      }) // {
  name = (drv.name + "-check");

  passthru = {
    inherit (drv) identifier config configFiles executableToolDepends cleanSrc env exeName;
  };

  inherit (drv) meta LANG LC_ALL buildInputs nativeBuildInputs;

  inherit (component) doCheck doCrossCheck;

  phases = ["unpackPhase" "patchPhase" "buildPhase"];

  # If doCheck or doCrossCheck are false we may still build this
  # component and we want it to quietly succeed.
  buildPhase = ''
    mkdir $out
    runHook preCheck

    ${toString component.testWrapper} ${drv}/bin/${drv.exeName} ${lib.concatStringsSep " " component.testFlags} | tee $out/test-stdout

    # Copy over tix files, if they exist
    find . -iname '${drv.exeName}.tix' -exec mkdir -p $out/share/hpc/vanilla/tix/${drv.exeName} \; -exec cp {} $out/share/hpc/vanilla/tix/${drv.exeName}/ \;

    runHook postCheck
  '';
} // haskellLib.optionalHooks {
  inherit (component) preCheck postCheck;
}
// lib.optionalAttrs (drv ? LOCALE_ARCHIVE) { inherit (drv) LOCALE_ARCHIVE; }
)

{ stdenv, lib, haskellLib, buildPackages }:
let self = drvOrig:

let
  # Work around problem running dynamicially linked Android executables with qemu.
  drv = drvOrig.override (oldAttrs: lib.optionalAttrs stdenv.hostPlatform.isAndroid { setupBuildFlags = oldAttrs.setupBuildFlags or [] ++ ["--ghc-option=-optl-static" ]; });

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
    inherit (drv) identifier config configFiles executableToolDepends cleanSrc env exeName meta;
    profiled = self drv.profiled;
    dwarf = self drv.dwarf;
  };

  inherit (drv) meta LANG LC_ALL buildInputs;

  nativeBuildInputs = drv.nativeBuildInputs
    ++ [buildPackages.xorg.lndir]
    ++ lib.optional (stdenv.hostPlatform.isGhcjs) buildPackages.nodejs;

  inherit (component) doCheck doCrossCheck;

  phases = ["unpackPhase" "patchPhase" "buildPhase"];

  # If doCheck or doCrossCheck are false we may still build this
  # component and we want it to quietly succeed.
  buildPhase = ''
    mkdir $out
    runHook preCheck

    drv=$(mktemp -d)
    lndir ${drv} $drv
    rm $drv/bin/${drv.exeName}
    cp ${drv}/bin/${drv.exeName} $drv/bin/${drv.exeName}
    patchShebangs --build $(dirname $drv/bin/${drv.exeName})
    ${toString component.testWrapper} $drv/bin/${drv.exeName} ${lib.concatStringsSep " " component.testFlags} | tee $out/test-stdout

    # Copy over tix files, if they exist
    find . -iname '${drv.exeName}.tix' -exec mkdir -p $out/share/hpc/vanilla/tix/${drv.exeName} \; -exec cp {} $out/share/hpc/vanilla/tix/${drv.exeName}/ \;

    runHook postCheck
  '';
} // haskellLib.optionalHooks {
  inherit (component) preCheck postCheck;
}
// lib.optionalAttrs (drv ? LOCALE_ARCHIVE) { inherit (drv) LOCALE_ARCHIVE; }
);
in self

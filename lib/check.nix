{ stdenv, lib, haskellLib, pkgsBuildBuild }:
let self = drvOrig:

# v2 slices don't have the v1-specific internals that this check
# wrapper relies on (drv.source, drv.drvAttrs, drv.configFiles,
# drv.testWrapper, ...).  For v2, just run the installed binary
# (which the slice builds to `$out/bin/<exeName>` but does not
# execute) and capture stdout.
if drvOrig.passthru.isSlice or false then
  let component = drvOrig.config;
      isTest = (drvOrig.identifier.component-type or "") == "test";
      testFlags = lib.concatStringsSep " " (component.testFlags or []);
      # Some host platforms can't execute their own binaries on
      # the build host directly: ghcjs needs `node`, windows-cross
      # needs wine.  v1's component config carries a `testWrapper`
      # list that v1's check phase prepends to the exe; reuse the
      # same shape here (it's set via the windows defaultModule
      # in `overlays/windows.nix`).
      testWrapper = component.testWrapper or [];
      testWrapperPrefix = lib.concatStringsSep " " testWrapper;
  in stdenv.mkDerivation ({
    name = drvOrig.name + "-check";
    passthru = { inherit (drvOrig) identifier config exeName meta; };
    inherit (drvOrig) exeName;
    inherit (component) doCheck doCrossCheck;
    # ghcjs binaries start with `#!/usr/bin/env node`.  Put node on
    # PATH and patch the shebang to its absolute path — same as the
    # v1 branch below.  Add every host-platform lib the slice was
    # linked against (via `passthru.runtimeLibs`) so test runners
    # like wineTestWrapper can find the runtime DLLs / dylibs the
    # exe imports — mirrors v1's automatic propagatedBuildInputs
    # chain.  Use `lib.getLib` so multi-output deps (`gmp.dev`,
    # `libffi.dev`, ...) resolve to the output that ships the
    # actual `.dll` / `.so` / `.dylib`, not the `-dev` headers.
    nativeBuildInputs =
      lib.optional stdenv.hostPlatform.isGhcjs pkgsBuildBuild.nodejs
      ++ map lib.getLib (drvOrig.passthru.runtimeLibs or []);
    phases = [ "buildPhase" ];
    buildPhase = ''
      mkdir -p $out
      runHook preCheck
      exe=${drvOrig}/bin/${drvOrig.exeName}
      ${lib.optionalString stdenv.hostPlatform.isGhcjs ''
        # The slice's $out is read-only; stage the binary into a
        # writable dir before patchShebangs can rewrite it.
        bindir=$(mktemp -d)
        cp "$exe" "$bindir/${drvOrig.exeName}"
        chmod +x "$bindir/${drvOrig.exeName}"
        patchShebangs --build "$bindir/${drvOrig.exeName}"
        exe=$bindir/${drvOrig.exeName}
      ''}
      ${testWrapperPrefix} "$exe" ${lib.optionalString isTest testFlags} | tee $out/test-stdout
      runHook postCheck
    '';
  } // haskellLib.optionalHooks {
    inherit (component) preCheck postCheck;
  })
else

let
  # Work around problem running dynamicially linked Android executables with qemu.
  drv = drvOrig.override (oldAttrs: lib.optionalAttrs stdenv.hostPlatform.isAndroid { setupBuildFlags = (oldAttrs.setupBuildFlags or []) ++ ["--ghc-option=-optl-static"]; });

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

  inherit (drv) LANG LC_ALL buildInputs;
  meta = builtins.removeAttrs drv.meta ["mainProgram"];

  nativeBuildInputs = drv.nativeBuildInputs
    ++ [(pkgsBuildBuild.lndir or pkgsBuildBuild.xorg.lndir)]
    ++ lib.optional (stdenv.hostPlatform.isGhcjs) pkgsBuildBuild.nodejs;

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

# The ghcForComponent function wraps ghc so that it is configured with
# the package database of all dependencies of a given component.
# It has been adapted from the ghcWithPackages wrapper in nixpkgs.
#
# This wrapper exists so that nix-shells for components will have a
# GHC automatically configured with the dependencies in the package
# database.

{ lib, stdenv, ghc, runCommand, lndir, makeWrapper
}:

{ componentName  # Full derivation name of the component
, configFiles    # The component's "config" derivation
, postInstall ? ""
}:

let
  inherit (configFiles) ghcCommand ghcCommandCaps packageCfgDir;
  libDir         = "$out/${configFiles.libDir}";
  docDir         = "$out/share/doc/ghc/html";
  # For musl we can use haddock from the buildGHC
  haddock        = if hostPlatform.isLinux && targetPlatform.isMusl
    then ghc.buildGHC
    else ghc;

in runCommand "${componentName}-${ghc.name}-env" {
  preferLocalBuild = true;
  passthru = {
    inherit (ghc) version meta;
    baseGhc = ghc;
  };
} (
  ''
    . ${makeWrapper}/nix-support/setup-hook

    # Start with a ghc...
    mkdir -p $out/bin
    ${lndir}/bin/lndir -silent ${ghc} $out
    # ... remove all of the package directories
    rm -rf ${libDir}/*/
    # ... but retain the lib/ghc/bin directory. This contains `unlit' and friends.
    ln -s ${ghc}/lib/${ghcCommand}-${ghc.version}/bin ${libDir}
    # ... and the ghcjs shim's if they are available ...
    if [ -d ${ghc}/lib/${ghcCommand}-${ghc.version}/shims ]; then
      ln -s ${ghc}/lib/${ghcCommand}-${ghc.version}/shims ${libDir}
    fi
    # ... and node modules ...
    if [ -d ${ghc}/lib/${ghcCommand}-${ghc.version}/ghcjs-node ]; then
      ln -s ${ghc}/lib/${ghcCommand}-${ghc.version}/ghcjs-node ${libDir}
    fi
    # Replace the package database with the one from target package config.
    ln -s ${configFiles}/${packageCfgDir} $out/${packageCfgDir}

    # Wrap compiler executables with correct env variables.
    # The NIX_ variables are used by the patched Paths_ghc module.

    for prg in ${ghcCommand} ${ghcCommand}i ${ghcCommand}-${ghc.version} ${ghcCommand}i-${ghc.version}; do
      if [[ -x "${ghc}/bin/$prg" ]]; then
        rm -f $out/bin/$prg
        makeWrapper ${ghc}/bin/$prg $out/bin/$prg                           \
          --add-flags '"-B$NIX_${ghcCommandCaps}_LIBDIR"'                   \
          --set "NIX_${ghcCommandCaps}"        "$out/bin/${ghcCommand}"     \
          --set "NIX_${ghcCommandCaps}PKG"     "$out/bin/${ghcCommand}-pkg" \
          --set "NIX_${ghcCommandCaps}_DOCDIR" "${docDir}"                  \
          --set "NIX_${ghcCommandCaps}_LIBDIR" "${libDir}"
      fi
    done

    for prg in runghc runhaskell; do
      if [[ -x "${ghc}/bin/$prg" ]]; then
        rm -f $out/bin/$prg
        makeWrapper ${ghc}/bin/$prg $out/bin/$prg                           \
          --add-flags "-f $out/bin/${ghcCommand}"                           \
          --set "NIX_${ghcCommandCaps}"        "$out/bin/${ghcCommand}"     \
          --set "NIX_${ghcCommandCaps}PKG"     "$out/bin/${ghcCommand}-pkg" \
          --set "NIX_${ghcCommandCaps}_DOCDIR" "${docDir}"                  \
          --set "NIX_${ghcCommandCaps}_LIBDIR" "${libDir}"
      fi
    done

    # Wrap haddock, if the base GHC provides it.
    if [[ -x "${haddock}/bin/haddock" ]]; then
      rm -f $out/bin/haddock
      makeWrapper ${haddock}/bin/haddock $out/bin/haddock    \
        --add-flags '"-B$NIX_${ghcCommandCaps}_LIBDIR"'  \
        --set "NIX_${ghcCommandCaps}_LIBDIR" "${libDir}"
    fi

    # Point ghc-pkg to the package database of the component using the
    # --global-package-db flag.

    for prg in ${ghcCommand}-pkg ${ghcCommand}-pkg-${ghc.version}; do
      if [[ -x "${ghc}/bin/$prg" ]]; then
        rm -f $out/bin/$prg
        makeWrapper ${ghc}/bin/$prg $out/bin/$prg --add-flags "--global-package-db=$out/${packageCfgDir}"
      fi
    done

    ${postInstall}
  ''
)

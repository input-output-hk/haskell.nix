# The ghcForComponent function wraps ghc so that it is configured with
# the package database of all dependencies of a given component.
# It has been adapted from the ghcWithPackages wrapper in nixpkgs.
#
# This wrapper exists so that nix-shells for components will have a
# GHC automatically configured with the dependencies in the package
# database.

{ lib, stdenv, ghc, runCommand, lndir, makeWrapper, haskellLib
}@defaults:

{ componentName  # Full derivation name of the component
, configFiles    # The component's "config" derivation
, postInstall ? ""
, enableDWARF
, plugins
, ghcOptions ? []
}:

let
  ghc = if enableDWARF then defaults.ghc.dwarf else defaults.ghc;

  inherit (configFiles) targetPrefix ghcCommand ghcCommandCaps packageCfgDir;
  libDir         = "$wrappedGhc/${configFiles.libDir}";
  docDir         = "$wrappedGhc/share/doc/ghc/html";
  # For musl we can use haddock from the buildGHC
  haddock        = if stdenv.hostPlatform.isMusl
    then ghc.buildGHC or ghc # `or ghc` is here because nixpkgs GHC does not have `buildGHC`
                             # TODO find a way to get suitable GHC and/or respect `ghc.hasHaddock`.
    else ghc;

  script = ''
    . ${makeWrapper}/nix-support/setup-hook

  ''
  # Start with a ghc and remove all of the package directories
  + ''
    mkdir -p $wrappedGhc/bin
    ${lndir}/bin/lndir -silent $unwrappedGhc $wrappedGhc
  ''
  + (
    if (builtins.compareVersions ghc.version "9.15" < 0) then ''
      rm -rf ${libDir}/*/
    ''
    else ''
      rm -rf ${libDir}/package.conf.d
    '')
  # ... but retain the lib/ghc/bin directory. This may contain `unlit' and friends.
  + ''
    if [ -d $unwrappedGhc/lib/${ghcCommand}-${ghc.version}/bin ]; then
      ln -s $unwrappedGhc/lib/${ghcCommand}-${ghc.version}/bin ${libDir}
    elif [ -d $unwrappedGhc/lib/bin ]; then
      ln -s $unwrappedGhc/lib/bin ${libDir}
    fi
  ''
  # ... and the ghcjs shim's if they are available ...
  + ''
    if [ -d $unwrappedGhc/lib/${ghcCommand}-${ghc.version}/shims ]; then
      ln -s $unwrappedGhc/lib/${ghcCommand}-${ghc.version}/shims ${libDir}
    fi
  ''
  # ... and node modules ...
  + ''
    if [ -d $unwrappedGhc/lib/${ghcCommand}-${ghc.version}/ghcjs-node ]; then
      ln -s $unwrappedGhc/lib/${ghcCommand}-${ghc.version}/ghcjs-node ${libDir}
    fi
  ''
  # Replace the package database with the one from target package config.
  + ''
    ln -s $configFiles/${packageCfgDir} $wrappedGhc/${packageCfgDir}

  ''
  # Set the GHC_PLUGINS environment variable according to the plugins for the component.
  # GHC will automatically load the relevant symbols from the given libraries and
  # initialize them with the given arguments.
  #
  # GHC_PLUGINS is a `read`able [(FilePath,String,String,[String])], where the
  # first component is a path to the shared library, the second is the package ID,
  # the third is the module name, and the fourth is the plugin arguments.
  + ''
    GHC_PLUGINS="["
    LIST_PREFIX=""
    ${builtins.concatStringsSep "\n" (map (plugin: ''
      id=$($unwrappedGhc/bin/ghc-pkg --package-db ${plugin.library}/package.conf.d field ${plugin.library.package.identifier.name} id --simple-output)
      lib_dir=$($unwrappedGhc/bin/ghc-pkg --package-db ${plugin.library}/package.conf.d field ${plugin.library.package.identifier.name} dynamic-library-dirs --simple-output)
      lib_base=$($unwrappedGhc/bin/ghc-pkg --package-db ${plugin.library}/package.conf.d field ${plugin.library.package.identifier.name} hs-libraries --simple-output)
      lib="$(echo ''${lib_dir}/lib''${lib_base}*)"
      GHC_PLUGINS="''${GHC_PLUGINS}''${LIST_PREFIX}(\"''${lib}\",\"''${id}\",\"${plugin.moduleName}\",["
      LIST_PREFIX=""
      ${builtins.concatStringsSep "\n" (map (arg: ''
        GHC_PLUGINS="''${GHC_PLUGINS}''${LIST_PREFIX}\"${arg}\""
        LIST_PREFIX=","
      '') plugin.args)}
      GHC_PLUGINS="''${GHC_PLUGINS}])"
      LIST_PREFIX=","
    '') plugins)}
    GHC_PLUGINS="''${GHC_PLUGINS}]"

  ''
  # now the tricky bit. For GHCJS (to make plugins work), we need a special
  # file called ghc_libdir. That points to the build ghc's lib.
  + ''
    echo "${ghc.buildGHC or ghc}/lib/${(ghc.buildGHC or ghc).name}" > "${libDir}/ghc_libdir"

  ''
  # Wrap compiler executables with correct env variables.
  # The NIX_ variables are used by the patched Paths_ghc module.
  + ''
    for prg in ${ghcCommand} ${ghcCommand}i ${ghcCommand}-${ghc.version} ${ghcCommand}i-${ghc.version}; do
      if [[ -x "$unwrappedGhc/bin/$prg" ]]; then
        rm -f $wrappedGhc/bin/$prg
        makeWrapper $unwrappedGhc/bin/$prg $wrappedGhc/bin/$prg                           \
          --add-flags '"-B$NIX_${ghcCommandCaps}_LIBDIR"'                   \
          --set "NIX_${ghcCommandCaps}"        "$wrappedGhc/bin/${ghcCommand}"     \
          --set "NIX_${ghcCommandCaps}PKG"     "$wrappedGhc/bin/${ghcCommand}-pkg" \
          --set "NIX_${ghcCommandCaps}_DOCDIR" "${docDir}"                  \
          --set "GHC_PLUGINS"                  "$GHC_PLUGINS"               \
          --set "NIX_${ghcCommandCaps}_LIBDIR" "${libDir}"${lib.concatMapStrings (o: " --add-flags ${o}") ghcOptions}
      fi
    done

    for prg in "${targetPrefix}runghc" "${targetPrefix}runhaskell"; do
      if [[ -x "$unwrappedGhc/bin/$prg" ]]; then
        rm -f $wrappedGhc/bin/$prg
        makeWrapper $unwrappedGhc/bin/$prg $wrappedGhc/bin/$prg                           \
          --add-flags "-f $wrappedGhc/bin/${ghcCommand}"                           \
          --set "NIX_${ghcCommandCaps}"        "$wrappedGhc/bin/${ghcCommand}"     \
          --set "NIX_${ghcCommandCaps}PKG"     "$wrappedGhc/bin/${ghcCommand}-pkg" \
          --set "NIX_${ghcCommandCaps}_DOCDIR" "${docDir}"                  \
          --set "GHC_PLUGINS"                  "$GHC_PLUGINS"               \
          --set "NIX_${ghcCommandCaps}_LIBDIR" "${libDir}"
      fi
    done

  ''
  + lib.optionalString (haskellLib.isNativeMusl && builtins.compareVersions ghc.version "9.9" >0) ''
     ln -s $wrappedGhc/bin/${targetPrefix}unlit $wrappedGhc/bin/unlit
     ln -s $wrappedGhc/bin/${ghcCommand}-iserv $wrappedGhc/bin/ghc-iserv
     ln -s $wrappedGhc/bin/${ghcCommand}-iserv-prof $wrappedGhc/bin/ghc-iserv-prof
  ''
  # These scripts break if symlinked (they check import.meta.filename against args)
  # Also for some reason `libdl.so` is missing `__wasm_apply_data_relocs`
  + lib.optionalString (stdenv.hostPlatform.isWasm) ''
     rm $wrappedGhc/lib/*.mjs
     cp $unwrappedGhc/lib/*.mjs $wrappedGhc/lib/
  ''
  # Wrap haddock, if the base GHC provides it.
  + ''
    if [[ -x "${haddock}/bin/haddock" ]]; then
      rm -f $wrappedGhc/bin/haddock
      makeWrapper ${haddock}/bin/haddock $wrappedGhc/bin/haddock    \
        --add-flags '"-B$NIX_${ghcCommandCaps}_LIBDIR"'  \
        --set "NIX_${ghcCommandCaps}_LIBDIR" "${libDir}"
    fi

  ''
  # Point ghc-pkg to the package database of the component using the
  # --global-package-db flag.
  + ''
    for prg in ${ghcCommand}-pkg ${ghcCommand}-pkg-${ghc.version}; do
      if [[ -x "$unwrappedGhc/bin/$prg" ]]; then
        rm -f $wrappedGhc/bin/$prg
        makeWrapper $unwrappedGhc/bin/$prg $wrappedGhc/bin/$prg --add-flags "--global-package-db=$wrappedGhc/${packageCfgDir}"
      fi
    done

    ${postInstall}
  '';

  drv = runCommand "${componentName}-${ghc.name}-env" {
  preferLocalBuild = true;
  passthru = {
    inherit script targetPrefix;
    inherit (ghc) version meta;
  };
  propagatedBuildInputs = configFiles.libDeps ++ lib.optional stdenv.hasCC stdenv.cc ++ [ghc];
} (''
    mkdir -p $out/configFiles
    configFiles=$out/configFiles
    ${configFiles.script}
    wrappedGhc=$out
    ${script}
'');
in {
  inherit script drv targetPrefix;
  baseGhc = ghc;
}

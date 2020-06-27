{ stdenv, lib, buildPackages, haskellLib, ghc, nonReinstallablePkgs, hsPkgs, makeSetupConfigFiles, pkgconfig }:

{ component, package, name, src, flags ? {}, revision ? null, patches ? [], defaultSetupSrc
, preUnpack ? component.preUnpack, postUnpack ? component.postUnpack
, prePatch ? null, postPatch ? null
, preBuild ? component.preBuild , postBuild ? component.postBuild
, preInstall ? component.preInstall , postInstall ? component.postInstall
, cleanSrc ? haskellLib.cleanCabalComponent package component src
}:

let
  fullName = "${name}-setup";

  includeGhcPackage = lib.any (p: p.identifier.name == "ghc") component.depends;

  configFiles = makeSetupConfigFiles {
    inherit (package) identifier;
    inherit fullName flags component;
  };
  hooks = haskellLib.optionalHooks {
    inherit
      preUnpack postUnpack
      prePatch postPatch
      preBuild postBuild
      preInstall postInstall
      ;
  };

  executableToolDepends =
    (lib.concatMap (c: if c.isHaskell or false
      then builtins.attrValues (c.components.exes or {})
      else [c]) component.build-tools) ++
    lib.optional (component.pkgconfig != []) pkgconfig;

  drv =
    stdenv.mkDerivation ({
      name = "${ghc.targetPrefix}${fullName}";
      src = cleanSrc;
      buildInputs = component.libs
        ++ component.frameworks
        ++ builtins.concatLists component.pkgconfig;
      nativeBuildInputs = [ghc] ++ executableToolDepends;

      passthru = {
        inherit (package) identifier;
        config = component;
        inherit configFiles cleanSrc;
      };

      meta = {
        homepage = package.homepage;
        description = package.synopsis;
        license =
          let
            license-map = import ../lib/cabal-licenses.nix lib;
          in license-map.${package.license} or
            (builtins.trace "WARNING: license \"${package.license}\" not found" license-map.LicenseRef-OtherLicense);
        platforms = if component.platforms == null then stdenv.lib.platforms.all else component.platforms;
      };

      phases = ["unpackPhase" "patchPhase" "buildPhase" "installPhase"];
      buildPhase = ''
        runHook preBuild
        if [[ ! -f ./Setup.hs  && ! -f ./Setup.lhs ]]; then
          cat ${defaultSetupSrc} > Setup.hs
        fi
        for f in Setup.hs Setup.lhs; do
          if [ -f $f ]; then
            echo Compiling package $f
            ghc $f -threaded '' + (if includeGhcPackage then "-package ghc " else "")
                + ''-package-db ${configFiles}/${configFiles.packageCfgDir} --make -o ./Setup
            setup=$(pwd)/Setup
          fi
        done
        [ -f ./Setup ] || (echo Failed to build Setup && exit 1)
        runHook preBuild
      '';

      installPhase = ''
        runHook preInstall
        mkdir -p $out/bin
        install ./Setup $out/bin/Setup
        runHook postInstall
      '';
    }
    // (lib.optionalAttrs (patches != []) { patches = map (p: if builtins.isFunction p then p { inherit (package.identifier) version; inherit revision; } else p) patches; })
    // hooks
  );
in drv
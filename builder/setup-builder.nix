{ pkgs, stdenv, lib, buildPackages, haskellLib, ghc, nonReinstallablePkgs, hsPkgs, makeSetupConfigFiles }@defaults:

let self =
{ component, package, name, src, enableDWARF ? false, flags ? {}, revision ? null, patches ? [], defaultSetupSrc
, preUnpack ? component.preUnpack, postUnpack ? component.postUnpack
, prePatch ? null, postPatch ? null
, preBuild ? component.preBuild , postBuild ? component.postBuild
, preInstall ? component.preInstall , postInstall ? component.postInstall
, cleanSrc ? haskellLib.cleanCabalComponent package component "setup" src
, nonReinstallablePkgs ? defaults.nonReinstallablePkgs
, smallAddressSpace ? false
}@drvArgs:

let
  ghc = (if enableDWARF then (x: x.dwarf) else (x: x)) (
        (if smallAddressSpace then (x: x.smallAddressSpace) else (x: x)) defaults.ghc);

  cleanSrc' = haskellLib.rootAndSubDir cleanSrc;

  fullName = "${name}-setup";

  includeGhcPackage = lib.any (p: p.identifier.name == "ghc") component.depends;

  configFiles = makeSetupConfigFiles {
    inherit (package) identifier;
    inherit fullName flags component enableDWARF nonReinstallablePkgs;
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
    lib.optional (component.pkgconfig != []) buildPackages.cabalPkgConfigWrapper;

  drv =
    stdenv.mkDerivation ({
      name = "${ghc.targetPrefix}${fullName}";
      src = cleanSrc'.root;
      buildInputs = component.libs
        ++ component.frameworks
        ++ builtins.concatLists component.pkgconfig;
      nativeBuildInputs = [ghc] ++ executableToolDepends;

      passthru = {
        inherit (package) identifier;
        config = component;
        srcSubDir = cleanSrc'.subDir;
        srcSubDirPath = cleanSrc'.root + cleanSrc'.subDir;
        cleanSrc = cleanSrc';
        inherit configFiles;
        dwarf = self (drvArgs // { enableDWARF = true; });
        smallAddressSpace = self (drvArgs // { smallAddressSpace = true; });
      };

      meta = {
        homepage = package.homepage or "";
        description = package.synopsis or "";
        license = haskellLib.cabalToNixpkgsLicense package.license;
        platforms = if component.platforms == null then lib.platforms.all else component.platforms;
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
            ghc $f -threaded ${if includeGhcPackage then "-package ghc " else ""
                }-package-db ${configFiles}/${configFiles.packageCfgDir} --make -o ./Setup
          fi
        done
        [ -f ./Setup ] || (echo Failed to build Setup && exit 1)
        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall
        mkdir -p $out/bin
        install ./Setup $out/bin/Setup
        runHook postInstall
      '';
    }
    // (lib.optionalAttrs (cleanSrc'.subDir != "") {
      prePatch =
        # If the package is in a sub directory `cd` there first
        ''
          cd ${lib.removePrefix "/" cleanSrc'.subDir}
        '';
    })
    // (lib.optionalAttrs (patches != []) { patches = map (p: if builtins.isFunction p then p { inherit (package.identifier) version; inherit revision; } else p) patches; })
    // hooks
  );
in drv; in self

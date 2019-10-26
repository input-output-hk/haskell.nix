{ stdenv, lib, buildPackages, haskellLib, ghc, nonReinstallablePkgs, hsPkgs, makeSetupConfigFiles, pkgconfig }:

{ component, package, name, src, flags, revision, patches, defaultSetupSrc
, preUnpack ? null, postUnpack ? null
}:

let
  cleanSrc = haskellLib.cleanCabalComponent package component src;

  fullName = "${name}-setup";

  includeGhcPackage = lib.any (p: p.identifier.name == "ghc") component.depends;

  configFiles = makeSetupConfigFiles {
    inherit (package) identifier;
    inherit fullName flags component;
  };
  hooks = haskellLib.optionalHooks {
    inherit preUnpack postUnpack;
  };

  executableToolDepends =
    (lib.concatMap (c: if c.isHaskell or false
      then builtins.attrValues (c.components.exes or {})
      else [c]) component.build-tools) ++
    lib.optional (component.pkgconfig != []) pkgconfig;

in
 stdenv.lib.fix (drv:
    stdenv.mkDerivation ({
      name = "${fullName}";
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

      phases = ["unpackPhase" "patchPhase" "buildPhase" "installPhase"];
      buildPhase = ''
        if [[ ! -f ./Setup.hs  && ! -f ./Setup.lhs ]]; then
          cat ${defaultSetupSrc} > Setup.hs
        fi
        for f in Setup.hs Setup.lhs; do
          if [ -f $f ]; then
            echo Compiling package $f
            ghc $f -threaded '' + (if includeGhcPackage then "-package ghc " else "")
                + ''-package-db ${configFiles}/package.conf.d --make -o ./Setup
            setup=$(pwd)/Setup
          fi
        done
        [ -f ./Setup ] || (echo Failed to build Setup && exit 1)
      '';

      installPhase = ''
        mkdir -p $out/bin
        install ./Setup $out/bin/Setup
      '';
    }
    // (lib.optionalAttrs (patches != []) { patches = map (p: if builtins.isFunction p then p { inherit (package.identifier) version; inherit revision; } else p) patches; })
    // hooks
  ))

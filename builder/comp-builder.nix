{ stdenv, ghc, lib, pkgconfig, writeText, haskellLib }:

{ componentId
, component
, package
, name
, setup
, src
, flags
, cabalFile
}:

let
  flagsAndConfig = field: xs: {
    flags = map (x: "--${field}=${x}") xs;
    config = lib.optional (xs != []) "${field}: ${lib.concatStringsSep " " xs}";
  };

  allFlagsAndConfigs = {
    packageDbs =
      let
        makePairs = map (p: rec { key="${val}"; val=p.components.${p.identifier.name}; });
        closure = builtins.genericClosure {
          startSet = makePairs component.depends;
          operator = {val,...}: makePairs val.config.depends;
        };
        flatDepends = map ({val,...}: val) closure;
      in flagsAndConfig "package-db" (map (p: "${p}/package.conf.d") flatDepends);

    extraLibDirs = flagsAndConfig "extra-lib-dirs" (map (p: "${p}/lib") component.libs);
    extraIncludeDirs = flagsAndConfig "extra-include-dirs" (map (p: "${lib.getDev p}/include") component.libs);
    extraFameworks = flagsAndConfig "extra-framework-dirs" (map (p: "${p}/Library/Frameworks") component.frameworks);
    userFlags = {
      flags = [("--flags=\"" + lib.concatStringsSep " " (lib.mapAttrsToList (fname: val: lib.optionalString (!val) "-" + fname) flags) + "\"")];
      config = [];
    };
  };

  finalConfigureFlags = lib.concatStringsSep " " (
    [ "--prefix=$out" "${componentId.ctype}:${componentId.cname}" ]
    ++ builtins.concatLists (lib.mapAttrsToList (_: x: x.flags) allFlagsAndConfigs)
    ++ component.configureFlags
  );
in stdenv.mkDerivation {
  name = "${name}-${componentId.ctype}-${componentId.cname}";

  inherit src;

  doCheck = componentId.ctype == "test";

  passthru = {
    inherit (package) identifier;
    config = component;
  };

  meta = {
    homepage = package.homepage;
    description = package.synopsis;
    license = (import ./cabal-licenses.nix lib).${package.license};
  };

  CABAL_CONFIG = writeText
    "package-db-cabal.config"
    (lib.concatStringsSep "\n" (builtins.concatLists (lib.mapAttrsToList (_: x: x.config) allFlagsAndConfigs)));

  enableParallelBuilding = true;

  buildInputs = component.libs
    ++ component.pkgconfig;

  nativeBuildInputs =
    [ghc]
    ++ lib.optional (component.pkgconfig != []) pkgconfig
    ++ lib.concatMap (c: if c.isHaskell or false
      then builtins.attrValues (c.components.exes or {})
      else [c]) component.build-tools;

  SETUP_HS = setup + /bin/Setup;

  # Phases
  prePatch = lib.optionalString (cabalFile != null) ''
    cat ${cabalFile} > ${package.identifier.name}.cabal
  '';

  configurePhase = ''
    echo Configure flags:
    printf "%q " ${finalConfigureFlags}
    echo
    $SETUP_HS configure ${finalConfigureFlags}
  '';

  buildPhase = ''
    $SETUP_HS build -j$NIX_BUILD_CORES
  '';

  checkPhase = ''
    $SETUP_HS test
  '';

  installPhase = ''
    $SETUP_HS copy
    ${lib.optionalString (haskellLib.isLibrary componentId) ''
      $SETUP_HS register --gen-pkg-config=${name}.conf
      ghc-pkg init $out/package.conf.d
      ghc-pkg ${lib.concatStringsSep " " allFlagsAndConfigs.packageDbs.flags} -f $out/package.conf.d register ${name}.conf
    ''}
  '';
}

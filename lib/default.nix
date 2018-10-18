{ lib, haskellLib }:

with haskellLib;

{
  subComponentTypes = [
    "sublibs"
    "foreignlibs"
    "exes"
    "tests"
    "benchmarks"
  ];

  mergeComponents = lib.zipAttrsWith (_: comps: lib.genAttrs [
    "depends"
    "libs"
    "frameworks"
    "pkgconfig"
    "build-tools"
  ] (n: {
    ${n} = lib.concatMap (comp: comp.${n} or []) comps;
  }));

  unionrAttrs = lib.foldr (a: b: a // b) {};

  mergeConfigs = configs:
    let
      mergeComponentType = type: mergeComponents (map (conf: (conf.components or {}).${type} or {}) configs);
    in unionrAttrs configs // {
      flags = unionrAttrs (map (conf: conf.flags or {}) configs);
      package = lib.foldr (conf: lib.recursiveUpdate (conf.package or {})) {} configs;
      components =
        mergeComponents (map (conf: removeAttrs (conf.components or {}) subComponentTypes) configs)
        // lib.genAttrs subComponentTypes mergeComponentType;
      configureFlags = concatMap (conf: conf.configureFlags or []) configs;
    };

  foldrAttrVals = f: z: attrs:
    lib.foldr (g: acc: g acc) z (lib.mapAttrsToList f attrs);

  attrLength = attrs: builtins.length (builtins.attrValues attrs);

  foldComponents = tys: f: z: conf:
    let
      comps = conf.components or {};
      foldSection = foldrAttrVals f;
      libComp = acc:
        foldSection acc
          (removeAttrs comps subComponentTypes);
      subComps = acc:
        lib.foldr
          (ty: acc': foldSection acc' (comps.${ty} or {}))
          acc
          tys;
    in libComp (subComps z);

  componentPrefix = {
    # Are all of these right?
    sublibs = "sublib";
    foreignlibs = "foreignlib";
    exes = "exe";
    tests = "test";
    benchmarks = "bench";
  };

  applyComponents = f: comps:
    let
      libComp = lib.mapAttrs (cname: f {ctype="lib"; inherit cname;}) (removeAttrs comps subComponentTypes);
      subComps = lib.mapAttrs
        (ctype: lib.mapAttrs (cname: f {inherit cname; ctype=componentPrefix.${ctype};}))
        (builtins.intersectAttrs (lib.genAttrs subComponentTypes (_: null)) comps);
    in subComps // libComp;

  isLibrary = componentId: componentId.ctype == "lib";

  adjustHackage = hackage: args:
    let
      handleVer = vdata:
        let
          revs = builtins.removeAttrs vdata ["sha256"];
          rev2HashedConfig = name: rev: { name = rev.cabalSha256; value = revConfigs.${name}; };
          revConfigs = lib.mapAttrs (_: rev2config vdata.sha256) revs;
        in revConfigs // lib.mapAttrs' rev2HashedConfig revs;
      rev2config = sha256: rev: self: import rev (args // { inherit (self) flags; }) // {
        inherit sha256;
        inherit (rev) cabalFile;
      };
    in lib.mapAttrs (_: lib.mapAttrs (_: handleVer)) hackage;

  buildPlan = { pkgs, plan }:
    let
      ghc = pkgs.haskell.compiler.${plan.compiler.nix-name};
      weakCallPackage = scope: f: args:
        let f' = if lib.isFunction f then f else import f;
            args' = scope // args;
        in f' (builtins.intersectAttrs (builtins.functionArgs f') args');

      new-builder = weakCallPackage pkgs ./builder {
        inherit haskellLib ghc weakCallPackage;
      };
      cabal = import ./cabal-os-arch-comp.nix;
      configs = lib.mapAttrs (_: lib.fix) plan.packages;
    in {
      system =
        let hostMap = import ./host-map.nix pkgs.stdenv;
        in cabal.os // { "is${hostMap.os}" = true; }
          // cabal.arch // { "is${hostMap.arch}" = true; };
      compiler = cabal.compiler // {
        isGhc = true;
        version = lib.mapAttrs (_: f: v: f (builtins.compareVersions plan.compiler.version v)) {
          eq = c: c == 0;
          gt = c: c > 0;
          ge = c: c >= 0;
          lt = c: c < 0;
          le = c: c <= 0;
        };
      };
      pkgconfPkgs = pkgs // {
        cairo-pdf = pkgs.cairo;
        cairo-ps = pkgs.cairo;
        cairo-svg = pkgs.cairo;
        xft = pkgs.xorg.libXft;
        xau = pkgs.xorg.libXau;
        libR = pkgs.R;
        fftw3f = pkgs.fftwFloat;
        fftw3 = pkgs.fftw;
      };
      pkgs = pkgs // {
        pthread = null;
        "stdc++" = null;
        ssl = pkgs.openssl.dev;
        crypto = pkgs.openssl.dev;
        z = pkgs.zlib;
        GL = pkgs.libGL;
        GLU = pkgs.libGLU;
        alut = pkgs.freealut;
        X11 = pkgs.xorg.libX11;
        Xrandr = pkgs.xorg.libXrandr;
        Xext = pkgs.xorg.libXext;
        Xi = pkgs.xorg.libXi;
        Xxf86vm = pkgs.xorg.libXxf86vm;
        Xcursor = pkgs.xorg.libXcursor;
        Xinerama = pkgs.xorg.libXinerama;
        mysqlclient = pkgs.mysql;
        Imlib2 = pkgs.imlib2;
        asound = pkgs.alsaLib;
        ffi = null;
      };
      hsPkgs = lib.mapAttrs (_: _: null) (plan.compiler.packages // { hsc2hs = "0.68.2"; })
        // lib.mapAttrs (_: new-builder) configs;
    };

}

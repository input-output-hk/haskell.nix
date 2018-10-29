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

  applyComponents = f: config:
    let
      comps = config.components;
      libComp = lib.mapAttrs (cname: f {ctype="lib"; cname=config.package.identifier.name;}) (removeAttrs comps subComponentTypes);
      subComps = lib.mapAttrs
        (ctype: lib.mapAttrs (cname: f {inherit cname; ctype=componentPrefix.${ctype};}))
        (builtins.intersectAttrs (lib.genAttrs subComponentTypes (_: null)) comps);
    in subComps // libComp;

  isLibrary = componentId: componentId.ctype == "lib";

  # Avoid pkgs.callPackage for now. It does a lot of nonsense with OOP
  # style programming that we should avoid until we know we want it.

  # weakCallPackage: call a function or (importable expression)
  # with scope + args.
  #
  # weakCallPackage scope f args
  #  will call f (scope // args)
  #
  # weakCallpackage scope ./path args
  #  will call the expression at ./path with (scope // args)
  #
  weakCallPackage = scope: f: args:
    let f' = if lib.isFunction f then f else import f;
        args' = scope // args;
    in f' (builtins.intersectAttrs (builtins.functionArgs f') args');
}

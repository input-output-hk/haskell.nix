{ lib, haskellLib }:

with haskellLib;

{
  # Within the package components, these are the attribute names of
  # nested attrsets.
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
      applyLibrary = cname: f { cname = config.package.identifier.name; ctype = "lib"; };
      applySubComp = ctype: cname: f { inherit cname; ctype = componentPrefix.${ctype}; };
      applyAllComp = f { cname = config.package.identifier.name; ctype = "all"; };
      libComp = lib.mapAttrs applyLibrary (removeAttrs comps (subComponentTypes ++ [ "all" ]));
      subComps = lib.mapAttrs
        (ctype: lib.mapAttrs (applySubComp ctype))
        (builtins.intersectAttrs (lib.genAttrs subComponentTypes (_: null)) comps);
      allComp = { all = applyAllComp comps.all; };
    in subComps // libComp // allComp;

  isLibrary = componentId: componentId.ctype == "lib";
  isAll = componentId: componentId.ctype == "all";
  isTest = componentId: componentId.ctype == "test";

  # Format a componentId as it should appear as a target on the
  # command line of the setup script.
  componentTarget = componentId:
    if componentId.ctype == "all" then ""
    else "${componentId.ctype}:${componentId.cname}";

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

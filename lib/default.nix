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

  foldrAttrVals = f: z: attrs:
    lib.foldr (g: acc: g acc) z (lib.mapAttrsToList (_name: f) attrs);

  foldComponents = tys: f: z: conf:
    let
      comps = conf.components or {};
      libComp = acc: if comps ? library then f comps.library acc else acc;
      subComps = acc:
        lib.foldr
          (ty: acc': foldrAttrVals f acc' (comps.${ty} or {}))
          acc
          tys;
    in libComp (subComps z);

  getAllComponents = foldComponents subComponentTypes (c: acc: [c] ++ acc) [];

  componentPrefix = {
    # Are all of these right?
    sublibs = "lib";
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

  # Remove null or empty values from an attrset.
  optionalHooks = lib.filterAttrs (_: hook: hook != null && hook != "");

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

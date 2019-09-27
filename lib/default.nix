{ lib, haskellLib, runCommand, git }:

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
      # ensure that comps.library exists and is not null.
      libComp = acc: if (comps.library or null) != null then f comps.library acc else acc;
      subComps = acc:
        lib.foldr
          (ty: acc': foldrAttrVals f acc' (comps.${ty} or {}))
          acc
          tys;
    in libComp (subComps z);

  getAllComponents = foldComponents subComponentTypes (c: acc:
    lib.optional c.buildable c ++ acc) [];

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
      applySubComp = ctype: cname: f { inherit cname; ctype = componentPrefix.${ctype} or (throw "Missing component mapping for ${ctype}."); };
      applyAllComp = f { cname = config.package.identifier.name; ctype = "all"; };
      buildableAttrs = lib.filterAttrs (n: comp: comp.buildable);
      libComp = if comps.library == null || !comps.library.buildable
        then {}
        else lib.mapAttrs applyLibrary (removeAttrs comps (subComponentTypes ++ [ "all" ]));
      subComps = lib.mapAttrs
        (ctype: attrs: lib.mapAttrs (applySubComp ctype) (buildableAttrs attrs))
        (builtins.intersectAttrs (lib.genAttrs subComponentTypes (_: null)) comps);
      allComp = { all = applyAllComp comps.all; };
    in subComps // libComp // allComp;

  isLibrary = componentId: componentId.ctype == "lib";
  isAll = componentId: componentId.ctype == "all";
  isTest = componentId: componentId.ctype == "test";
  isBenchmark = componentId: componentId.ctype == "bench";

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
        args' = (builtins.intersectAttrs (builtins.functionArgs f') scope) // args;
    in f' args';

  # Collect all (transitive) Haskell library dependencies of a
  # component.
  ## flatLibDepends :: Component -> [Package]
  flatLibDepends = component:
    let
      makePairs = map (p: rec { key="${val}"; val=(p.components.library or p); });
      closure = builtins.genericClosure {
        startSet = makePairs component.depends;
        operator = {val,...}: makePairs val.config.depends;
      };
    in map ({val,...}: val) closure;

  # Extracts a selection of components from a Haskell package set.
  #
  # This can be used to filter out all test suites or benchmarks of
  # your project, so that they can be built in Hydra.
  #
  # For example:
  #
  #    tests = collectComponents "tests" (package: package.identifier.name == "mypackage") hsPkgs;
  #
  # Will result in moving:
  #   from: hsPkgs.mypackage.components.tests.unit-tests
  #     to: tests.mypackage.unit-tests
  #
  collectComponents = group: packageSel: haskellPackages:
    (lib.mapAttrs (_: package: package.components.${group} // { recurseForDerivations = true; })
     (lib.filterAttrs (name: package: (package.isHaskell or false) && packageSel package) haskellPackages))
    // { recurseForDerivations = true; };

  # Replacement for lib.cleanSourceWith that has a subDir argument.
  inherit (import ./clean-source-with.nix { inherit lib; }) cleanSourceWith canCleanSource;

  # Use cleanSourceWith to filter just the files needed for a particular
  # component of a package
  cleanCabalComponent = import ./clean-cabal-component.nix { inherit lib cleanSourceWith; };

  # Clean git directory based on `git ls-files --recurse-submodules`
  cleanGit = import ./clean-git.nix {
    inherit lib runCommand git cleanSourceWith;
  };
}

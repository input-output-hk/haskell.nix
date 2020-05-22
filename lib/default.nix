{ pkgs, stdenv, lib, haskellLib, recurseIntoAttrs, srcOnly }:


with haskellLib;

let
  # Why `final.evalPackages.buildPackages.git`?
  # Why not just final.evalPackages.git?
  #
  # A problem arises when `evalPackages` is `buildPackages`.i
  # As may be the case in a flake.
  #
  # It turns out `git` depends on `gdb` in a round about way:
  #  git -> openssh -> libfido2 -> systemd -> python libxml -> Cython -> gdb
  # Somewhere in that chain there should perhaps be a `buildPackages` so
  # that the `gdb` that is used is not the one for debugging code in
  # the `final` (but instead the one for debugging code in
  # `final.buildPackages`).
  #
  # Using `final.buildPackages.git` causes two problems:
  #
  #   * Multiple versions of `git` (and that dependency chain
  #     to `gdb` are needed when cross compiling).
  #   * When `gdb` does not exist for `js`, so when cross
  #     compiling with ghcjs `final.buildPackages.git` fails
  #     to build at all.
  inherit (pkgs.evalPackages.buildPackages) git;

in {
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
    sublibs = "lib";
    foreignlibs = "flib";
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
      buildableAttrs = lib.filterAttrs (n: comp: comp.buildable or true);
      libComp = if comps.library == null || !(comps.library.buildable or true)
        then {}
        else lib.mapAttrs applyLibrary (removeAttrs comps (subComponentTypes ++ [ "all" "setup" ]));
      subComps = lib.mapAttrs
        (ctype: attrs: lib.mapAttrs (applySubComp ctype) (buildableAttrs attrs))
        (builtins.intersectAttrs (lib.genAttrs subComponentTypes (_: null)) comps);
      allComp = { all = applyAllComp comps.all; };
    in subComps // libComp // allComp;

  isLibrary = componentId: componentId.ctype == "lib";
  isAll = componentId: componentId.ctype == "all";
  isExe = componentId: componentId.ctype == "exe";
  isTest = componentId: componentId.ctype == "test";
  isBenchmark = componentId: componentId.ctype == "bench";
  isExecutableType = componentId:
       isExe componentId
    || isTest componentId
    || isBenchmark componentId;
  mayHaveExecutable = componentId:
       isExecutableType componentId
    || isAll componentId;

  # Was there a reference to the package source in the `cabal.project` or `stack.yaml` file.
  # This is used to make the default `packages` list for `shellFor`.
  isLocalPackage = p: p.isLocal or false;
  selectLocalPackages = ps: lib.filterAttrs (n: p: p != null && isLocalPackage p) ps;

  # if it's a project package it has a src attribute set with an origSubDir attribute.
  # project packages are a subset of localPackages
  isProjectPackage = p: p ? src && p.src ? origSubDir;
  selectProjectPackages = ps: lib.filterAttrs (n: p: p != null && isLocalPackage p && isProjectPackage p) ps;

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
      # this is a minor improvement over the "cannot coerce set to string"
      # error.  It will now say:
      #
      # > The option `packages.Win32.package.identifier.name' is used but not defined.
      #
      # which indicates that the package.Win32 is missing and not defined.
      getKey = x: if x ? "outPath" then "${x}" else (throw x.identifier.name);
      makePairs = map (p: rec { key=getKey val; val=(p.components.library or p); });
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
    let packageToComponents = name: package:
          # look for the components with this group if there are any
          let components = package.components.${group} or {};
          # set recurseForDerivations unless it's a derivation itself (e.g. the "library" component) or an empty set
          in if lib.isDerivation components || components == {}
             then components
             else recurseIntoAttrs components;
        packageFilter = name: package: (package.isHaskell or false) && packageSel package;
        filteredPkgs = lib.filterAttrs packageFilter haskellPackages;
        # at this point we can filter out packages that don't have any of the given kind of component
        packagesByComponent = lib.filterAttrs (_: components: components != {}) (lib.mapAttrs packageToComponents filteredPkgs);
    in recurseIntoAttrs packagesByComponent;

  # Equivalent to collectComponents with (_: true) as selection function.
  # Useful for pre-filtered package-set.
  #
  # For example:
  #
  #    myHaskellPackages = selectProjectPackages hsPkgs;
  #    myTests = collectComponents' "tests" myHaskellPackages;
  collectComponents' = group: collectComponents group (_: true);

  # Extracts a selection of 'checks' from a Haskell package set.
  #
  # This can be used to collect all the test runs in your project, so that can be run in CI.
  collectChecks = packageSel: haskellPackages:
    let packageFilter = name: package: (package.isHaskell or false) && packageSel package;
    in recurseIntoAttrs (lib.mapAttrs (_: p: p.checks) (lib.filterAttrs packageFilter haskellPackages));

  # Equivalent to collectChecks with (_: true) as selection function.
  # Useful for pre-filtered package-set.
  collectChecks' = collectChecks (_: true);

  # Replacement for lib.cleanSourceWith that has a subDir argument.
  inherit (import ./clean-source-with.nix { inherit lib; }) cleanSourceWith canCleanSource;

  # Use cleanSourceWith to filter just the files needed for a particular
  # component of a package
  cleanCabalComponent = import ./clean-cabal-component.nix { inherit lib cleanSourceWith; };

  # Clean git directory based on `git ls-files --recurse-submodules`
  cleanGit = import ./clean-git.nix {
    inherit lib git cleanSourceWith;
    inherit (pkgs.evalPackages) runCommand;
  };

  # Check a test component
  check = import ./check.nix {
    inherit stdenv lib haskellLib srcOnly;
  };

  # Use `isCrossHost` to identify when we are cross compiling and
  # the code we are producing will not run on the build system
  # without an emulator.
  # In most cases we do not want to treat musl as a cross compiler.
  # For instance when building ghc we want to include ghci.
  isCrossHost = stdenv.hostPlatform != stdenv.buildPlatform
    && !(stdenv.buildPlatform.isLinux && stdenv.hostPlatform.isMusl && stdenv.buildPlatform.isx86 && stdenv.hostPlatform.isx86);
  # This is the same as isCrossHost but for use when building ghc itself
  isCrossTarget = stdenv.targetPlatform != stdenv.hostPlatform
    && !(stdenv.hostPlatform.isLinux && stdenv.targetPlatform.isMusl && stdenv.hostPlatform.isx86 && stdenv.targetPlatform.isx86);
  # Native musl build-host-target combo
  isNativeMusl = stdenv.hostPlatform.isMusl
    && stdenv.buildPlatform == stdenv.hostPlatform
    && stdenv.hostPlatform == stdenv.targetPlatform;

  # Takes a version number or attr set of arguments (for cabalProject)
  # and conversios it to an attr set of argments.  This allows
  # the use of "1.0.0.0" or { version = "1.0.0.0"; ... }
  versionOrArgsToArgs = versionOrArgs:
    if lib.isAttrs versionOrArgs
      then versionOrArgs
      else { version = versionOrArgs; };
}

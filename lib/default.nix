{ pkgs, stdenv, lib, haskellLib, recurseIntoAttrs, srcOnly }:


with haskellLib;

let
  # Why `final.evalPackages.buildPackages.gitMinimal`?
  # Why not just final.evalPackages.gitMinimal?
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
  inherit (pkgs.evalPackages.buildPackages) gitMinimal;

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

  getAllComponents = foldComponents subComponentTypes (c: acc: [c] ++ acc) [];

  componentPrefix = {
    sublibs = "lib";
    foreignlibs = "flib";
    exes = "exe";
    tests = "test";
    benchmarks = "bench";
  };

  # For looking up the components attribute based on the cabal component type
  prefixComponent =
    lib.listToAttrs (
      lib.mapAttrsToList (value: name: { inherit name value; })
        componentPrefix);

  applyComponents = f: config:
    let
      comps = config.components;
      applyLibrary = cname: f { cname = config.package.identifier.name; ctype = "lib"; };
      applySubComp = ctype: cname: f { inherit cname; ctype = componentPrefix.${ctype} or (throw "Missing component mapping for ${ctype}."); };
      isBuildable = comp:
        config.package.buildable # Set manually in a module (allows whole packages to be disabled)
        && comp.buildable        # Set based on `buildable` in `.cabal` files
        && comp.planned;         # Set if the component was in the `plan.json`
      buildableAttrs = lib.filterAttrs (n: isBuildable);
      libComp = if comps.library == null || !(isBuildable comps.library)
        then {}
        else lib.mapAttrs applyLibrary (removeAttrs comps (subComponentTypes ++ [ "setup" ]));
      subComps = lib.mapAttrs
        (ctype: attrs: lib.mapAttrs (applySubComp ctype) (buildableAttrs attrs))
        (builtins.intersectAttrs (lib.genAttrs subComponentTypes (_: null)) comps);
    in subComps // libComp;

  isLibrary = componentId: componentId.ctype == "lib";
  isExe = componentId: componentId.ctype == "exe";
  isTest = componentId: componentId.ctype == "test";
  isBenchmark = componentId: componentId.ctype == "bench";
  isExecutableType = componentId:
       isExe componentId
    || isTest componentId
    || isBenchmark componentId;
  mayHaveExecutable = componentId:
       isExecutableType componentId;

  # Was there a reference to the package source in the `cabal.project` or `stack.yaml` file.
  # This is used to make the default `packages` list for `shellFor`.
  isLocalPackage = p: p.isLocal or false;
  selectLocalPackages = ps: lib.filterAttrs (n: p: p != null && isLocalPackage p) ps;

  # if it's a project package it has a src attribute set with an origSubDir attribute.
  # project packages are a subset of localPackages
  isProjectPackage = p: p.isProject or false;
  selectProjectPackages = ps: lib.filterAttrs (n: p: p != null && isLocalPackage p && isProjectPackage p) ps;

  # Format a componentId as it should appear as a target on the
  # command line of the setup script.
  componentTarget = componentId:"${componentId.ctype}:${componentId.cname}";

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
      makePairs = map (p: rec { key=val.name; val=(p.components.library or p); });
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
  cleanCabalComponent = import ./clean-cabal-component.nix { inherit lib cleanSourceWith canCleanSource; };

  # Clean git directory based on `git ls-files --recurse-submodules`
  cleanGit = import ./clean-git.nix {
    inherit lib cleanSourceWith;
    git = gitMinimal;
    inherit (pkgs.evalPackages.buildPackages) runCommand;
  };

  # Some times it is handy to temporarily use a relative path between git
  # repos.  If the repos are individually cleaned this is not possible
  # (since the cleaned version of one repo will never include the files
  # of the other).
  #
  # `cleanGits` allows us to specify a root directory and any number of
  # sub directories containing git repos.
  #
  # See docs/user-guide/clean-git.md for details of how to use this
  # with `cabalProject`.
  cleanGits = { src, gitDirs, name ? null, caller ? "cleanGits" }@args:
    let
      # List of filters, one for each git directory.
      filters = builtins.map (subDir:
        (pkgs.haskell-nix.haskellLib.cleanGit {
          src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
            inherit src subDir;
          };
        }).filter) gitDirs;
    in pkgs.haskell-nix.haskellLib.cleanSourceWith {
      inherit src name caller;
      # Keep files that match any of the filters
      filter = path: type: pkgs.lib.any (f: f path type) filters;
    };

  # Check a test component
  check = import ./check.nix {
    inherit stdenv lib haskellLib srcOnly;
  };

  # Do coverage of a package
  coverageReport = import ./cover.nix {
    inherit stdenv lib haskellLib pkgs;
  };

  # Do coverage of a project
  projectCoverageReport = import ./cover-project.nix {
    inherit stdenv lib haskellLib pkgs;
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
  # and converts it to an attr set of arguments.  This allows
  # the use of "1.0.0.0" or { version = "1.0.0.0"; ... }
  versionOrArgsToArgs = versionOrArgs:
    if lib.isAttrs versionOrArgs
      then versionOrArgs
      else { version = versionOrArgs; };

  # Find the resolver in the stack.yaml file and fetch it if a sha256 value is provided
  fetchResolver = import ./fetch-resolver.nix {
    inherit (pkgs.evalPackages) pkgs;
  };

  inherit (import ./cabal-project-parser.nix {
    inherit pkgs;
  }) parseIndexState parseBlock;


  cabalToNixpkgsLicense = import ./spdx/cabal.nix pkgs;

  # This function is like
  #   `src + (if subDir == "" then "" else "/" + subDir)`
  # however when `includeSiblings` is set it maintains
  # `src.origSrc` if there is one and instead adds to
  # `src.origSubDir`.  It uses `cleanSourceWith` when possible
  # to keep `cleanSourceWith` support in the result.
  appendSubDir = { src, subDir, includeSiblings ? false }:
    if subDir == ""
      then src
      else
        if haskellLib.canCleanSource src
          then haskellLib.cleanSourceWith {
            inherit src subDir includeSiblings;
          }
          else let name = src.name or "source" + "-" + __replaceStrings ["/"] ["-"] subDir;
            in if includeSiblings
              then rec {
                # Keep `src.origSrc` so it can be used to allow references
                # to other parts of that root.
                inherit name;
                origSrc = src.origSrc or src;
                origSubDir = src.origSubDir or "" + "/" + subDir;
                outPath = origSrc + origSubDir;
              }
              else {
                # We are not going to need other parts of `origSrc` if there
                # was one and we can ignore it
                inherit name;
                outPath = src + "/" + subDir;
              };

  # Givin a `src` split it into a `root` path (based on `src.origSrc` if
  # present) and `subDir` (based on `src.origSubDir).  The
  # `root` will still use the `filter` of `src` if there was one.
  rootAndSubDir = src: rec {
    subDir = src.origSubDir or "";
    root =
      if subDir == ""
        then src # if there was no subdir use the original src
        else
          # Use `cleanSourceWith` to make sure the `filter` is still used
          if src ? origSrc && src ? filter
            then haskellLib.cleanSourceWith {
              name = src.name or "source" + "-root";
              src = src.origSrc;
              # Not passing src.origSubDir so that the result points `origSrc`
              inherit (src) filter;
            }
            else src.origSrc or src;  # If there is a subDir and origSrc (but no filter) use origSrc
  };

  # Run evalModules passing the project function argument (m) as a module along with
  # the the a projectType module (../modules/cabal-project.nix or ../modules/stack-project.nix).
  # The resulting config is then passed to the project function's implementation.
  evalProjectModule = projectType: m: f:
    let project = f
      (lib.evalModules {
        modules = (if builtins.isList m then m else [m]) ++ [
          # Include ../modules/cabal-project.nix or ../modules/stack-project.nix
          (import ../modules/project-common.nix)
          (import projectType)
          # Pass the pkgs and the buildProject to the modules
          ({ config, lib, ... }: {
            _module.args = {
              inherit pkgs;
              # to make it easy to depends on build packages in, eg., shell definition:
              inherit (project) buildProject;
            };
            inherit (project) hsPkgs;
          })
        ];
      }).config;
    in project;

  # Converts from a `compoent.depends` value to a library derivation.
  # In the case of sublibs the `depends` value should already be the derivation.
  dependToLib = d: d.components.library or d;
}

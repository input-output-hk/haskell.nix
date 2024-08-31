{ pkgs, stdenv, lib, haskellLib, recurseIntoAttrs, srcOnly }:


with haskellLib;

let
  # Why `final.buildPackages.buildPackages.gitMinimal`?
  # Why not just final.buildPackages.gitMinimal?
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
  inherit (pkgs.buildPackages.buildPackages) gitMinimal;

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
    lib.foldr f z (builtins.attrValues attrs);

  foldComponents = tys: f: z: conf:
    let
      comps = conf.components or { };
      # ensure that comps.library exists and is not null.
      libComp = acc:
        if comps ? library then f comps.library acc else acc;
      subComps = acc:
        lib.foldr
          (ty: acc': foldrAttrVals f acc' (comps.${ty} or { }))
          acc
          tys;
    in
    libComp (subComps z);

  getAllComponents = foldComponents subComponentTypes (c: acc: [ c ] ++ acc) [ ];

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
      buildableAttrs = lib.filterAttrs (_n: isBuildable);
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
  mayHaveExecutable = isExecutableType;

  # Was there a reference to the package source in the `cabal.project` or `stack.yaml` file.
  # This is used to make the default `packages` list for `shellFor`.
  isLocalPackage = p: p.isLocal or false;
  selectLocalPackages = lib.filterAttrs (_n: p: p != null && isLocalPackage p);

  # if it's a project package it has a src attribute set with an origSubDir attribute.
  # project packages are a subset of localPackages
  isProjectPackage = p: p.isProject or false;
  selectProjectPackages = lib.filterAttrs (n: p: p != null && !(p.isRedirect or false) && isLocalPackage p && isProjectPackage p);

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
      makePairs = map (p: rec { key=val.name; val=p.components.library or p; });
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
    let packageToComponents = _name: package:
          # look for the components with this group if there are any
          let components = package.components.${group} or {};
          # set recurseForDerivations unless it's a derivation itself (e.g. the "library" component) or an empty set
          in if lib.isDerivation components || components == {}
             then components
             else recurseIntoAttrs components;
        packageFilter = _name: package: (package.isHaskell or false) && packageSel package;
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
    let packageFilter = _name: package: (package.isHaskell or false) && packageSel package;
    in recurseIntoAttrs (lib.mapAttrs (_: p: p.checks) (lib.filterAttrs packageFilter haskellPackages));

  # Equivalent to collectChecks with (_: true) as selection function.
  # Useful for pre-filtered package-set.
  collectChecks' = collectChecks (_: true);

  # Replacement for lib.cleanSourceWith that has a subDir argument.
  inherit (import ./clean-source-with.nix { inherit lib; }) cleanSourceWith;

  # Use cleanSourceWith to filter just the files needed for a particular
  # component of a package
  cleanCabalComponent = import ./clean-cabal-component.nix { inherit lib cleanSourceWith; };

  # Clean git directory based on `git ls-files --recurse-submodules`
  cleanGit = import ./clean-git.nix {
    inherit lib cleanSourceWith;
    git = gitMinimal;
    inherit (pkgs.buildPackages.buildPackages) runCommand;
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
  cleanGits = { src, gitDirs, name ? null, caller ? "cleanGits" }:
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
    inherit stdenv lib haskellLib;
    inherit (pkgs) buildPackages;
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
    && !(stdenv.buildPlatform.isLinux && stdenv.hostPlatform.isMusl && stdenv.buildPlatform.linuxArch == stdenv.hostPlatform.linuxArch);
  # This is the same as isCrossHost but for use when building ghc itself
  isCrossTarget = stdenv.targetPlatform != stdenv.hostPlatform
    && !(stdenv.hostPlatform.isLinux && stdenv.targetPlatform.isMusl && stdenv.hostPlatform.linuxArch == stdenv.targetPlatform.linuxArch);
  # Native musl build-host-target combo
  isNativeMusl = stdenv.hostPlatform.isMusl
    && stdenv.buildPlatform == stdenv.hostPlatform
    && stdenv.hostPlatform == stdenv.targetPlatform;

  # Takes a version number, module or list of modules (for cabalProject)
  # and converts it to an list of project modules.  This allows
  # the use of "1.0.0.0" or { version = "1.0.0.0"; ... }
  versionOrModToMods = versionOrMod:
    if lib.isString versionOrMod
      then [{ version = versionOrMod; }]
    else if lib.isList versionOrMod
      then versionOrMod
    else [versionOrMod];

  # Find the resolver in the stack.yaml file and fetch it if a sha256 value is provided
  fetchResolver = import ./fetch-resolver.nix {
    inherit (pkgs.buildPackages) pkgs;
  };

  inherit (import ./cabal-project-parser.nix {
    inherit pkgs;
  }) parseIndexState parseSourceRepositoryPackages parseRepositories parseSourceRepositoryPackageBlock parseRepositoryBlock;


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
        haskellLib.cleanSourceWith {
          inherit src subDir includeSiblings;
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
      });
    in project;

  # Converts from a `compoent.depends` value to a library derivation.
  # In the case of sublibs the `depends` value should already be the derivation.
  dependToLib = d:
    # Do simplify this to `d.components.library or d`, as that
    # will not give a good error message if the `.library`
    # is missing (happens if the package is unplanned,
    # but has overrides).
    # It would be nice to put an `assert` here, but there is
    # currently no good way to get the name of the dependency
    # when it is not in the plan.  The attribute path of
    # `d` in the `nix` error should include the name
    # eg. `packages.Cabal.components.library`.
    if d ? components
      then d.components.library
      else d;

  projectOverlays = import ./project-overlays.nix {
    inherit lib haskellLib;
  };

  # Use by `prefixFlake` to add a prefix to every attribute
  prefixAttrs = prefix: x:
    __listToAttrs (map (n:{
      name = prefix + n;
      value = x.${n};
    }) (__attrNames x));

  # Used by `combineFlakes` to add the prefix to each flake
  prefixFlake = prefix: sep: flake:
    if prefix == "default"
      then flake
      else
        __mapAttrs (_: prefixAttrs (prefix + sep)) flake //
        lib.optionalAttrs (flake ? devShell) {
          # We can't add the prefix to this
          inherit (flake) devShell;
        } // lib.optionalAttrs (flake ? devShells) {
          devShells = __listToAttrs (map (n: {
            # We don't want ":default" on the end of the non
            # default dev shells
            name = if n == "default"
              then prefix
              else prefix + sep + n;
            value = flake.devShells.${n};
          }) (__attrNames flake.devShells));
        } // lib.optionalAttrs (flake ? hydraJobs) {
          hydraJobs.${lib.removeSuffix ":" prefix} = flake.hydraJobs;
        } // lib.optionalAttrs (flake ? ciJobs) {
          ciJobs.${lib.removeSuffix ":" prefix} = flake.ciJobs;
        };

  # Used by `combineFlakes` to combine flakes together
  addFlakes = a: b:
    __listToAttrs (map (name: {
      inherit name;
      value =
        # This favours the first item (`a`) in the case of duplicates
        # so that `combineFlakes` will use the first flake in the
        # list for `devShell`.
        if name == "devShell"
          then a.devShell or b.devShell # `devShell` is a derivation
        else
          (b.${name} or {}) // (a.${name} or {});
    }) (__attrNames (a // b)));

  # This function can combine a list of flakes allong with
  # suitable prefix values into a single flake.
  # Since thre is no way to add a prefix to `devShell`, the first
  # one in the list will be used.
  combineFlakes = sep: prefixAndFlakes: builtins.foldl' addFlakes {}
    (lib.mapAttrsToList (prefix: flake: prefixFlake prefix sep flake) prefixAndFlakes);

  # Make the CI jobs for running code coverage.
  # `project` is the base project without code coverage enabled.
  # `packages` is a selector function that indicates what packages
  # we should run code coverage on (pass haskellLib.selectProjectPackages
  # to run it on the packages).
  # `coverageProjectModule` is applied to `project` and is useful for
  # modifying the project settings when running code coverage (just
  # pass `{}` if you do not need to modify anything).
  # By default the `doCoverage` flag will be set for the packages
  # selected by `packages`.
  projectCoverageCiJobs = project: packages: coverageProjectModule:
    let
      packageNames = project: builtins.attrNames (packages project.hsPkgs);
      coverageProject = project.appendModule [
        coverageProjectModule
        {
          modules = [{
            packages = lib.genAttrs (packageNames project)
              (_: { doCoverage = lib.mkDefault true; });
          }];
        }
      ];
    in
      builtins.listToAttrs (lib.concatMap (packageName: [{
        name = packageName;
        value = coverageProject.hsPkgs.${packageName}.coverageReport;
      }]) (packageNames coverageProject));

  # Flake package names that are flat and match the cabal component names.
  mkFlakePackages =
    foldrAttrVals
      (package: acc:
        foldComponents
          subComponentTypes
          (component: a: a // {
            ${component.passthru.identifier.component-id} = component;
          })
          acc
          package)
      { };

  # Flake package names that are flat and match the cabal component names.
  mkFlakeApps =
    foldrAttrVals
      (package: acc:
        foldComponents
          [ "exes" "tests" "benchmarks" ]
          (component: a: a // {
            ${component.passthru.identifier.component-id} = {
              type = "app";
              program = component.exePath;
            };
          })
          acc
          package)
      { };

  # Flatten the result of collectChecks or collectChecks' for use in flake `checks`
  mkFlakeChecks = allChecks:
    foldrAttrVals
      (package: acc:
        foldrAttrVals
          (check: a: a // {
            ${check.passthru.identifier.component-id} = check;
          })
          acc
          package)
      { }
      (removeRecurseForDerivations allChecks);

  removeRecurseForDerivations = x:
    let clean = builtins.removeAttrs x ["recurseForDerivations"];
    in
      if x.recurseForDerivations or false
        then builtins.mapAttrs (_: removeRecurseForDerivations) clean
        else clean;

  mkFlakeCiJobs = project: {
          packages
        , checks
        , coverage
        , devShells
        , checkedProject
    }: {
      # Run all the tests and code coverage
      checks = removeRecurseForDerivations checks;
      inherit
        coverage
        # Make sure all the packages build
        packages
        # Build and cache any tools in the `devShells`
        devShells;
      # Build tools and cache tools needed for the project
      inherit (project) roots;
    }
    # Build the plan-nix and check it if materialized
    // lib.optionalAttrs (checkedProject ? plan-nix) {
      inherit (checkedProject) plan-nix;
    }
    # Build the stack-nix and check it if materialized
    // lib.optionalAttrs (checkedProject ? stack-nix) {
      inherit (checkedProject) stack-nix;
    };

  mkFlake = project: {
          selectPackages ? haskellLib.selectProjectPackages
        , haskellPackages ? selectPackages project.hsPkgs
        , packages ? mkFlakePackages haskellPackages
        , apps ? mkFlakeApps haskellPackages
        , checks ? mkFlakeChecks (collectChecks' haskellPackages)
        , coverage ? {}
        , devShell ? project.shell
        , devShells ? { default = devShell; }
        , checkedProject ? project.appendModule { checkMaterialization = true; }
        , ciJobs ? mkFlakeCiJobs project { inherit checks coverage packages devShells checkedProject; }
        , hydraJobs ? ciJobs
      }: {
      inherit
          # Used by:
          #   `nix build .#pkg-name:lib:pkg-name`
          #   `nix build .#pkg-name:lib:sublib-name`
          #   `nix build .#pkg-name:exe:exe-name`
          #   `nix build .#pkg-name:test:test-name`
          packages
          # Used by:
          #   `nix flake check`
          checks
          #   `nix run .#pkg-name:exe:exe-name`
          #   `nix run .#pkg-name:test:test-name`
          apps
          # Used by hydra.
          hydraJobs
          # Like `hydraJobs` but with `${system}` first so that it the IFDs will not have
          # to run for systems we are not testing (placement of `${system}` is done
          # by `flake-utils.eachSystem` and it treats `hydraJobs` differently from
          # the other flake attributes).
          # See https://github.com/numtide/flake-utils/blob/04c1b180862888302ddfb2e3ad9eaa63afc60cf8/default.nix#L131-L134
          ciJobs
          # Used by:
          #   `nix develop`
          devShells
          devShell; # TODO remove devShell once everyone has nix that supports `devShells.default`
      };

  # Adapt a standard project shell (`project.shell` or `haskell-nix.shellFor`)
  # into a devshell module (https://github.com/numtide/devshell)
  # that should provide the same environnement.
  devshellFor = shell: {
    packages = lib.filter lib.isDerivation (shell.nativeBuildInputs
    # devshell does not use pkgs.mkShell / pkgs.stdenv.mkDerivation,
    # so we need to explicit required dependencies which
    # are provided implicitely by stdenv when using the normal shell:
    ++ shell.stdenv.defaultNativeBuildInputs)
    ++ [shell.stdenv.cc.bintools];
    # We need to expose all the necessary env variables:
    env = [
      {
        name = "PKG_CONFIG_PATH";
        value = lib.makeSearchPath "lib/pkgconfig" shell.buildInputs;
      }
    ] ++ lib.mapAttrsToList lib.nameValuePair ({
      inherit (shell) NIX_GHC_LIBDIR;
    # CABAL_CONFIG is only set if the shell was built with exactDeps=true
    } // lib.optionalAttrs (shell ? CABAL_CONFIG) {
      inherit (shell) CABAL_CONFIG;
    });
  };

  makeCompilerDeps = import ./make-compiler-deps.nix {
    inherit (pkgs.buildPackages.buildPackages) lib runCommand;
  };

  # Here we try to figure out which qemu to use based on the host platform.
  # This guess can be overridden by passing qemuSuffix
  qemuByHostPlatform = hostPlatform:
    # I'd prefer this was a dictionary lookup, with a fall through into abort,
    # that would make this more readable I guess.  I think there is some similar
    # mapping somewhere in haskell.nix
    if hostPlatform.isAarch32
    then "arm"
    else if hostPlatform.isAarch64
    then "aarch64"
    else abort "Don't know which QEMU to use for hostPlatform ${hostPlatform.config}. Please provide qemuSuffix";

  # How to run ldd when checking for static linking
  lddForTests = "${pkgs.pkgsBuildBuild.glibc.bin}/bin/ldd";

  # Version of `lib.unique` that should be fast if the name attributes are unique
  uniqueWithName = list:
    lib.concatMap lib.unique (
      builtins.attrValues (
        builtins.groupBy (x: if __typeOf x == "set" then x.name or "noname" else "notset") list));

  # Assert that each item in the list is unique
  checkUnique = msg: x:
    if __length x == __length (uniqueWithName x)
      then x
      else builtins.throw "Duplicate items found in ${msg} ${
        __toJSON (__attrNames (lib.filterAttrs (_: v: __length v > 1) (
          builtins.groupBy (x: if __typeOf x == "set" then x.name or "noname" else "notset") x)))
      }";

  types = import ./types.nix { inherit lib; };
}

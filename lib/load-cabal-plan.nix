{haskellLib, pkgs}:
{callProjectResults, selectedCompiler}:
let
  # Read the plan.json file `plan-nix` derivation
  plan-json = builtins.fromJSON (
    builtins.unsafeDiscardStringContext (
      builtins.readFile (callProjectResults.projectNix + "/plan.json")));
  # All the units in the plan indexed by unit ID.
  by-id = pkgs.lib.listToAttrs (map (x: { name = x.id; value = x; }) plan-json.install-plan);
  # Find the names of all the pre-existing packages used by a list of dependencies
  # (includes transitive dependencies)
  lookupPreExisting = depends:
    pkgs.lib.concatMap (d: builtins.attrNames pre-existing-depends.${d}) depends;
  pre-existing-depends =
    pkgs.lib.listToAttrs (map (p: {
      name = p.id;
      value = pkgs.lib.optionalAttrs (p.type == "pre-existing") { ${p.pkg-name} = null; } //
        pkgs.lib.listToAttrs (
          map (dname: { name = dname; value = null; }) (lookupPreExisting (p.depends or p.components.lib.depends)));
    }) plan-json.install-plan);
  # Lookup a dependency in `hsPkgs`
  lookupDependency = hsPkgs: d:
    pkgs.lib.optional (by-id.${d}.type != "pre-existing") (
        if by-id.${d}.component-name or "lib" == "lib"
          then hsPkgs.${d} or hsPkgs."${by-id.${d}.pkg-name}-${by-id.${d}.pkg-version}" or hsPkgs.${by-id.${d}.pkg-name}
          else hsPkgs.${d}.components.sublibs.${pkgs.lib.removePrefix "lib:" by-id.${d}.component-name});
  # Lookup an executable dependency in `hsPkgs.pkgsBuildBuild`
  lookupExeDependency = hsPkgs: d:
    # Try to lookup by ID, but if that fails use the name (currently a different plan is used by pkgsBuildBuild when cross compiling)
    (hsPkgs.pkgsBuildBuild.${d} or hsPkgs.pkgsBuildBuild.${by-id.${d}.pkg-name}).components.exes.${pkgs.lib.removePrefix "exe:" by-id.${d}.component-name};
  # Populate `depends`, `pre-existing` and `build-tools`
  lookupDependencies = hsPkgs: depends: exe-depends: {
    depends = pkgs.lib.concatMap (lookupDependency hsPkgs) depends;
    pre-existing = lookupPreExisting depends;
    build-tools = map (lookupExeDependency hsPkgs) exe-depends;
  };
  # Calculate the packages for a component
  getComponents = cabal2nixComponents: hsPkgs: p:
    let
      components = p.components or { ${p.component-name or "lib"} = { inherit (p) depends; exe-depends = p.exe-depends or []; }; };
      # Other than the `lib` and `setup` components, component names
      # have a prefix based on their type.
      componentsWithPrefix = collectionName: prefix:
        pkgs.lib.listToAttrs (pkgs.lib.concatLists (pkgs.lib.mapAttrsToList (n: c:
          pkgs.lib.optional (pkgs.lib.hasPrefix "${prefix}:" n) (
            let
              name = pkgs.lib.removePrefix "${prefix}:" n;
              value = (if cabal2nixComponents == null then {} else cabal2nixComponents.${collectionName}.${name}) // {
                buildable = true;
              } // lookupDependencies hsPkgs c.depends c.exe-depends;
            in { inherit name value; }
          )) components));
    in
      pkgs.lib.mapAttrs componentsWithPrefix haskellLib.componentPrefix
      // pkgs.lib.optionalAttrs (components ? lib) {
        library = (if cabal2nixComponents == null then {} else cabal2nixComponents.library) // {
          buildable = true;
        } // lookupDependencies hsPkgs components.lib.depends components.lib.exe-depends;
      } // pkgs.lib.optionalAttrs (components ? setup) {
        setup = {
          buildable = true;
        } // lookupDependencies hsPkgs.pkgsBuildBuild (components.setup.depends or []) (components.setup.exe-depends or []);
      };
  nixFilesDir = callProjectResults.projectNix + callProjectResults.src.origSubDir or "";
in {
  # This replaces the `plan-nix/default.nix`
  pkgs = (hackage: {
    packages = pkgs.lib.listToAttrs (
      # Include entries for the `pre-existing` packages, but leave them as `null`
      pkgs.lib.concatMap (p:
        pkgs.lib.optional (p.type == "pre-existing") {
          name = p.id;
          value.revision = null;
        }) plan-json.install-plan
      # The other packages that are not part of the project itself.
      ++ pkgs.lib.concatMap (p:
        pkgs.lib.optional (p.type == "configured" && (p.style == "global" || p.style == "inplace") ) {
          name = p.id;
          value.revision =
            {hsPkgs, ...}@args:
              let
                # Read the output of `Cabal2Nix.hs`.  We need it for information not
                # in the `plan.json` file.
                cabal2nix = (
                  if builtins.pathExists (nixFilesDir + "/cabal-files/${p.pkg-name}.nix")
                    then import (nixFilesDir + "/cabal-files/${p.pkg-name}.nix")
                  else if builtins.pathExists (nixFilesDir + "/.plan.nix/${p.pkg-name}.nix")
                    then import (nixFilesDir + "/.plan.nix/${p.pkg-name}.nix")
                  else (((hackage.${p.pkg-name}).${p.pkg-version}).revisions).default) (args // { hsPkgs = {}; });
              in pkgs.lib.optionalAttrs (p ? pkg-src-sha256) {
                sha256 = p.pkg-src-sha256;
              } // pkgs.lib.optionalAttrs (p.pkg-src.type or "" == "source-repo") {
                # Replace the source repository packages with versions created when
                # parsing the `cabal.project` file.
                src = pkgs.lib.lists.elemAt callProjectResults.sourceRepos (pkgs.lib.strings.toInt p.pkg-src.source-repo.location)
                  + pkgs.lib.optionalString (p.pkg-src.source-repo.subdir != ".") "/${p.pkg-src.source-repo.subdir}";
              } // pkgs.lib.optionalAttrs (cabal2nix ? package-description-override && p.pkg-version == cabal2nix.package.identifier.version) {
                # Use the `.cabal` file from the `Cabal2Nix` if it for the matching
                # version of the package (the one in the plan).
                inherit (cabal2nix) package-description-override;
              } // {
                flags = p.flags; # Use the flags from `plan.json`
                components = getComponents cabal2nix.components hsPkgs p;
                package = cabal2nix.package // {
                  identifier = { name = p.pkg-name; version = p.pkg-version; id = p.id; };
                  isProject = false;
                  setup-depends = []; # The correct setup depends will be in `components.setup.depends`
                };
              };
        }) plan-json.install-plan);
    compiler = {
      inherit (selectedCompiler) version;
    };
  });
  # Packages in the project (those that are both configure and local)
  extras = (_hackage: {
    packages = pkgs.lib.listToAttrs (
      pkgs.lib.concatMap (p:
        pkgs.lib.optional (p.type == "configured" && p.style == "local") {
          name = p.id;
          value =
            {hsPkgs, ...}@args:
              let cabal2nix = import (nixFilesDir + "/.plan.nix/${p.pkg-name}.nix") (args // { hsPkgs = {}; });
              in pkgs.lib.optionalAttrs (p ? pkg-src-sha256) {
                sha256 = p.pkg-src-sha256;
              } // pkgs.lib.optionalAttrs (p.pkg-src.type or "" == "local" && cabal2nix ? cabal-generator) {
                inherit (cabal2nix) cabal-generator;
              } // pkgs.lib.optionalAttrs (p.pkg-src.type or "" == "local") {
                # Find the `src` location based on `p.pkg-src.path`
                src = if pkgs.lib.hasPrefix "/" p.pkg-src.path
                  then p.pkg-src.path # Absolute path
                  else haskellLib.appendSubDir {
                    # Relative to the project path
                    inherit (callProjectResults) src;
                    subDir = pkgs.lib.removePrefix "./" (pkgs.lib.removePrefix "/" (pkgs.lib.removeSuffix "/." (pkgs.lib.removeSuffix "/." (
                      if pkgs.lib.hasPrefix ".${callProjectResults.src.origSubDir or ""}/" (p.pkg-src.path + "/")
                        then pkgs.lib.removePrefix ".${callProjectResults.src.origSubDir or ""}" p.pkg-src.path
                        else throw "Unexpected path ${p.pkg-src.path} expected it to start with .${callProjectResults.src.origSubDir or ""}"))));
                    includeSiblings = true; # Filtering sibling dirs of the package dir is done in the
                                            # component builder so that relative paths can be used to
                                            # reference project directories not in the package subDir.
                  };
              } // {
                flags = p.flags; # Use the flags from `plan.json`
                components = getComponents cabal2nix.components hsPkgs p;
                package = cabal2nix.package // {
                  identifier = { name = p.pkg-name; version = p.pkg-version; id = p.id; };
                  isProject = true;
                  setup-depends = []; # The correct setup depends will be in `components.setup.depends`
                };
              };
        }) plan-json.install-plan);
  });
  modules = [
    { inherit plan-json; }
    (import ../modules/install-plan/non-reinstallable.nix)
    (import ../modules/install-plan/override-package-by-name.nix)
    (import ../modules/install-plan/planned.nix { inherit getComponents; })
    (import ../modules/install-plan/redirect.nix)
  ];
}

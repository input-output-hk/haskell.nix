{ overrides, pkgs, shell, stackage }: project: root:

with pkgs;
with lib;

let
  inherit (haskell.lib) overrideCabal;

  stackagePackages = (import stackage) stackagePackages pkgs;

  resolverName = replaceStrings ["."] [""] project.resolver;
  resolver = stackagePackages.haskell.packages.stackage."${resolverName}";

  snapshot = resolver.override {
    overrides = mergeExtensions [
      defaultDeps
      extraDeps
      localDeps
      overrides
    ];
  };

  inherit (snapshot) callHackage;

  defaultDeps = final: previous: {
    mkDerivation = drv: previous.mkDerivation (drv // {
      doCheck = false;
      doHaddock = false;
      enableExecutableProfiling = false;
      enableLibraryProfiling = false;
    });
  };

  handlers = import ./handlers.nix pkgs;

  handleExtra = spec:
    let
      handler = findFirst (h: h.test spec)
        (throw "can't handle extra dep: ${spec}") handlers;
    in
    handler.handle spec;

  extraSpecs = project.extra-deps or [];

  extraDeps =
    mergeExtensions (map (spec: final: const (handleExtra spec final)) extraSpecs);

  withStrictDeps = drv: drv.overrideAttrs (const { strictDeps = true; });

  localPackage = name: path:
    let
      drv = cabalToNix snapshot name root {} ''--subpath="${path}"'';
      overrides = const {
        doBenchmark = true;
        doCheck = true;
        doHaddock = true;
        license = licenses.free;
      };
    in
    withStrictDeps (overrideCabal drv overrides);

  localAttrs = listToAttrs
    (map (path: nameValuePair (cabalPackageName "${root}/${path}") path) (project.packages or ["."]));

  localDeps = final: previous:
    mapAttrs localPackage localAttrs;

  target = mapAttrs (name: const (getAttr name snapshot)) localAttrs;

  localPaths = map (removePrefix "./") (project.packages or ["."]);

  stackSnapshot = {
    inherit (project) resolver;
    packages = extraSpecs;
  };

  pathHash = path:
    builtins.unsafeDiscardStringContext (builtins.substring 0 32 (baseNameOf path));

  stackSnapshotWithName = stackSnapshot // {
    name = pathHash (exportYAML stackSnapshot);
  };

  stackConfig = {
    packages = project.packages or ["."];
    resolver = exportYAML stackSnapshotWithName;
  };

  shellEnv = snapshot.shellFor {
    packages = const (attrValues target);
    nativeBuildInputs = [ cabal-install stack ];

    STACK_IN_NIX_SHELL = 1;
    STACK_IN_NIX_EXTRA_ARGS = "";
    STACK_PLATFORM_VARIANT = "nix";
    STACK_YAML = "stack-to-nix.yaml";

    shellHook = ''
      cat ${exportYAML stackConfig} > stack-to-nix.yaml

      for f in $(find * -name package.yaml); do
        ${snapshot.hpack}/bin/hpack --force $f
      done

      echo packages: > cabal.project
      for spec in ${concatStringsSep " " localPaths}; do
        echo "  $spec" >> cabal.project
      done
    '';
  };
in

if shell then shellEnv else target

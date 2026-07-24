# Project-global data derived once from plan.json for the v2 builder.
#
# `projectConfigPragmas`, the documentation-enabled package set, and the
# per-package source-repo flag map all depend ONLY on the install plan
# (plus the project's ghc version and host platform) — never on any
# individual component.  comp-v2-builder needs them while emitting every
# slice's `cabal.project`, and computing them inline meant re-scanning
# the whole plan (and re-running the configure-arg regexes) once per
# component — which dominated v2 evaluation (hundreds of millions of
# `builtins.match` / `lib.elem` calls on a project the size of
# cardano-node).
#
# So we compute them ONCE here and memoise the result behind the
# project-level module option `config.plan-json-v2-globals` (see
# `modules/install-plan/override-package-by-name.nix`), then pass it into
# every slice via hspkg-builder.
{ lib }:

{ planJson      # plan.json's `install-plan` array
, isWasm        # stdenv.hostPlatform.isWasm
, ghcVersion    # ghc.version
, rawCabalProject ? null
                # assembled cabal.project + cabalProjectLocal text the
                # plan was made from, or null when the caller can't
                # provide it (legacy pkg-set instantiations).  Used to
                # disambiguate the `-haddock` configure-arg marker —
                # see `projectSetsDocumentation` below.
}:
let
  # ---- per-package source-repo flags (was `planFlagsFor`) ----------
  # For source-repository-package units, module-system flag overrides
  # don't reach cabal-install at plan time, so the slice must reproduce
  # the flag assignment plan-nix actually saw — plan-json's flags.
  # Hackage / repo-tar / local packages keep the module-flag behaviour
  # (resolved per-slice, not here).  Keyed on the *first* unit of each
  # pkg-name, matching the old `lib.findFirst` semantics.
  firstUnitByName = lib.foldl'
    (acc: p:
      let n = p.pkg-name or null;
      in if n == null || acc ? ${n} then acc else acc // { ${n} = p; })
    {} planJson;
  flagsByName = lib.filterAttrs (_: v: v != null)
    (lib.mapAttrs
      (_: unit:
        let srcType = (unit.pkg-src or {}).type or null;
        in if srcType == "source-repo" then unit.flags or {} else null)
      firstUnitByName);

  # ---- documentation-enabled package names (was `pkgDocEnabled`) ----
  # `documentation: True` adds `--ghc-option=-haddock` to a unit's
  # configure-args; a name is "doc-enabled" if any of its units carry it.
  #
  # The marker is ambiguous: a plain `ghc-options: -haddock` in the
  # project (e.g. haskell-language-server's `package *` stanza) leaves
  # the IDENTICAL configure-arg, but its elaborated units hash with
  # `pkgHashDocumentation = False` — emitting `documentation: True` for
  # them forks the slice's UnitId from plan-nix AND makes `cabal
  # v2-build` run the haddock program (which stable-haskell compilers
  # don't ship).  plan.json has no truthful per-unit documentation
  # field (`haddock-args` is always the bare verb), so disambiguate
  # with the project text: only treat `-haddock` as documentation when
  # some line of the assembled cabal.project actually turns
  # `documentation:` on.  Coarse (a doc-True project that ALSO gives
  # some package plain `ghc-options: -haddock` still misclassifies
  # that package, and `import:`ed files are not scanned) — the real
  # fix is make-install-plan emitting `elabBuildHaddocks`, at the cost
  # of a nix-tools (world-rebuild) change.  With rawCabalProject
  # unknown (null) keep the old marker-only behaviour.
  projectSetsDocumentation =
    rawCabalProject == null
    || lib.any
         (l: builtins.isString l
             && builtins.match "[ \t]*documentation[ \t]*:[ \t]*[Tt]rue[ \t\r]*" l != null)
         (builtins.split "\n" rawCabalProject);
  docEnabledNames =
    if !projectSetsDocumentation then {}
    else lib.listToAttrs (lib.concatMap
      (e:
        if lib.elem "--ghc-option=-haddock" (e.configure-args or [])
        then [ { name = e.pkg-name or ""; value = true; } ]
        else [])
      planJson);

  # ---- projectConfigPragmas (was the inline `let` in comp-v2) ------
  # Each plan entry's `configure-args` is per-unit; we group by pkg-name
  # (union of pragmas across units, since cabal.project only supports
  # per-package granularity) and emit a `package *` baseline block plus
  # per-package delta blocks, so the slice's `cabal v2-build` keeps
  # `pkgHashConfigInputs` aligned with plan-nix.
  configuredEntries = lib.filter (p: (p.type or "") == "configured") planJson;

  # Whitelist of cabal.project field names that map 1:1 to
  # `--enable-X` / `--disable-X` / `--X=value` Setup configure args.
  isProjectField = field: lib.elem field [
    "shared" "static"
    "library-vanilla" "library-profiling"
    "executable-dynamic" "executable-static"
    "profiling" "profiling-shared"
    "optimization" "debug-info" "build-info"
    "library-for-ghci"
    "split-sections" "split-objs"
    "executable-stripping" "library-stripping"
    "coverage" "relocatable"
    "profiling-detail" "library-profiling-detail"
  ];
  # On wasm GHC 9.12+ the RTS linker only supports shared libraries, so
  # plan-nix's `--disable-shared` would break TH-eval `dyld`; flip it.
  forceShared = isWasm && builtins.compareVersions ghcVersion "9.12" >= 0;
  pragmaOf = arg:
    let
      en = builtins.match "--enable-([a-z0-9-]+)" arg;
      di = builtins.match "--disable-([a-z0-9-]+)" arg;
      kv = builtins.match "--([a-z0-9-]+)=(.+)" arg;
    in
      if forceShared && arg == "--disable-shared" then "shared: True"
      # `debug-info` is a LEVEL (0..3), not a boolean: its cabal.project value is
      # parsed by `flagToDebugInfoLevel`, which rejects True/False ("Can't parse
      # debug info level False").  This matters with the stable-haskell Cabal
      # fork (3.17); older Cabal accepted the boolean form.  Map the enable/
      # disable Setup flags to the equivalent level (disable → 0 = NoDebugInfo,
      # matching `--disable-debug-info`; enable → 2 = NormalDebugInfo, the
      # no-arg default) so `pkgHashConfigInputs` stays aligned with plan-nix.
      else if di != null && builtins.head di == "debug-info" then "debug-info: 0"
      else if en != null && builtins.head en == "debug-info" then "debug-info: 2"
      else if en != null && isProjectField (builtins.head en) then "${builtins.head en}: True"
      else if di != null && isProjectField (builtins.head di) then "${builtins.head di}: False"
      else if kv != null && isProjectField (builtins.head kv) then "${builtins.head kv}: ${builtins.elemAt kv 1}"
      else null;
  pragmasOf = args: lib.unique (lib.filter (s: s != null) (map pragmaOf args));

  # pkg-name → union of pragmas across every unit of that package.
  pragmasByName = lib.foldl' (acc: e:
    let
      name = e.pkg-name or "";
      ps = pragmasOf (e.configure-args or []);
    in if name == "" then acc
       else acc // { ${name} = lib.unique ((acc.${name} or []) ++ ps); }
  ) {} configuredEntries;
  allPkgNames = builtins.attrNames pragmasByName;

  # Baseline: pragmas present in *every* pkg-name's union.
  baseline =
    if allPkgNames == [] then []
    else
      let firstPkg = builtins.head allPkgNames;
      in lib.filter
           (p: lib.all (n: lib.elem p pragmasByName.${n}) allPkgNames)
           pragmasByName.${firstPkg};
  baselineSet = lib.listToAttrs (map (p: { name = p; value = true; }) baseline);

  baselineBlock =
    if baseline == [] then ""
    else "package *\n"
       + lib.concatMapStrings (p: "  " + p + "\n") baseline;

  perPkgBlocks = lib.concatMapStrings (name:
    let delta = lib.filter (p: !(baselineSet ? ${p})) pragmasByName.${name};
    in if delta == [] then ""
       else "package ${name}\n"
          + lib.concatMapStrings (p: "  " + p + "\n") delta
  ) allPkgNames;

  projectConfigPragmas = baselineBlock + perPkgBlocks;
in {
  inherit projectConfigPragmas docEnabledNames flagsByName;
}

# How haskell.nix's own tests consume head.hackage.
#
# `test/cabal.project.local` keeps head.hackage's real root keys, so that a
# plain `cabal` build driven by that file still verifies the published
# repository.  haskell.nix does not use the published one: it builds the
# repository itself (see overlays/head-hackage.nix), because the published one
# is regenerated on a schedule and is not reproducible, so a `--sha256` pin
# against it went stale roughly daily.
#
# That locally built copy is signed with keys generated at build time, so the
# published root keys cannot apply to it.  They are dropped here rather than
# weakened in the checked-in file, which stays correct for everyone who is not
# going through haskell.nix.
{ evalPackages
  # The compiler in use ships NO target boot libraries and builds them from
  # source instead.  Drops the parts of cabal.project.local that assume
  # otherwise -- see `bootLibsFromSource` below for what and why, and
  # test/default.nix for who asks.
, bootLibsFromSource ? false
}:
{
  # The `assert` is the point of writing it this way.  If the stanza in
  # cabal.project.local is reformatted this stops matching, and we want that to
  # fail here rather than silently leave the real keys in place -- which would
  # fail much later inside cabal, as an opaque signature error.
  cabalProjectLocal =
    let
      raw = builtins.readFile ./cabal.project.local;
      # Written with explicit newlines rather than `''`-strings: there, the
      # replacement's indentation is relative to the source, so `key-threshold`
      # reads as though it were nested under `root-keys:` when it is not.  It is
      # a sibling, both at two spaces -- the same shape as the ghcjs-overlay
      # stanza already in cabal.project.local.
      published =
        "key-threshold: 3\n"
        + "  root-keys:\n"
        + "     f76d08be13e9a61a377a85e2fb63f4c5435d40f8feb3e12eb05905edb8cdea89\n"
        + "     26021a13b401500c8eb2761ca95c61f2d625bfef951b939a8124ed12ecf07329\n"
        + "     7541f32a4ccca4f97aea3b22f5e593ba2c0267546016b992dfadcd2fe944e55d\n";
      servedLocally =
        "root-keys:\n"
        + "  key-threshold: 0\n";
      stripped = builtins.replaceStrings [ published ] [ servedLocally ] raw;

      # Two things in the `os(ghcjs)` part of this file assume the compiler
      # ships its target boot libraries.  A stable-haskell `-target` compiler
      # does not: its target package db is empty (`emptyGlobalPackageDb`), the
      # boot packages are injected as `packages:` sources, and the boot deps
      # are pinned to the versions its own stage2 project uses.  Both then
      # contradict the project rather than help it, and the solver stops before
      # reaching anything the test is actually about.
      #
      # (1) The ghcjs overlay is a package repository activated with
      # `:override`, so for every package it carries it replaces hackage
      # outright -- that is what it is for, patching packages that do not build
      # for the JS backend.  It carries `unix-2.8.1.0`; the pins ask for
      # `unix ==2.8.8.0`; the injected boot project's `Cabal` needs `unix` on
      # any non-Windows host:
      #
      #   rejecting: host:unix == host:source:unix-2.8.1.0
      #     (constraint from cabal.project requires ==2.8.8.0)
      #
      # Dropped from `active-repositories` rather than from the `repository`
      # stanza, so the file still declares the same repositories and a plain
      # `cabal` run driven by it is unaffected.
      #
      # (2) `constraints: ghci installed` cannot hold when nothing is
      # installed.  `libraries/ghci` is one of the injected boot packages, so
      # it is a local source package and the only `ghci` at the right version:
      #
      #   rejecting: host:ghci == host:source:ghci-9.14
      #     (constraint from project config cabal.project requires installed
      #      instance)
      #
      # The neighbouring `extra-packages: ghci` stays.  It makes `ghci` a goal
      # so the JS external interpreter has it for Template Haskell, which is
      # just as true here -- only the demand that it come pre-installed is
      # wrong.
      #
      # Both use the same `assert` discipline as the root keys above: if these
      # lines are reformatted we want to hear about it here, not as an opaque
      # solver failure much later.
      drop = needle: s:
        let out = builtins.replaceStrings [ needle ] [ "" ] s;
        in assert out != s; out;
      forFromSourceBootLibs =
        drop "  constraints: ghci installed\n"
          (drop ", ghcjs-overlay:override" stripped);
    in
      assert stripped != raw;
      if bootLibsFromSource then forFromSourceBootLibs else stripped;

  # Scoped to our own tests deliberately: a haskell.nix user whose project names
  # this url should still get the published repository, not ours.
  #
  # Taken from `evalPackages`, not `pkgs`: this is an eval-time derivation, like
  # plan-to-nix, and belongs on the eval system.  Reached through `pkgs` it came
  # out built for the target's build platform, so evaluating the x86_64-linux
  # jobs produced an x86_64-linux derivation and CI tried to build it on the
  # linux VMs instead of the darwin builders that do eval-time work.
  inputMap = {
    "https://ghc.gitlab.haskell.org/head.hackage/" =
      evalPackages.haskell-nix.head-hackage-repo;
  };
}

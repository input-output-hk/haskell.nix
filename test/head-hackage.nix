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
{ pkgs }:
{
  # The `assert` is the point of writing it this way.  If the stanza in
  # cabal.project.local is reformatted this stops matching, and we want that to
  # fail here rather than silently leave the real keys in place -- which would
  # fail much later inside cabal, as an opaque signature error.
  cabalProjectLocal =
    let
      raw = builtins.readFile ./cabal.project.local;
      stripped = builtins.replaceStrings [''
        key-threshold: 3
          root-keys:
             f76d08be13e9a61a377a85e2fb63f4c5435d40f8feb3e12eb05905edb8cdea89
             26021a13b401500c8eb2761ca95c61f2d625bfef951b939a8124ed12ecf07329
             7541f32a4ccca4f97aea3b22f5e593ba2c0267546016b992dfadcd2fe944e55d
      ''] [''
        root-keys:
          key-threshold: 0
      ''] raw;
    in
      assert stripped != raw; stripped;

  # Scoped to our own tests deliberately: a haskell.nix user whose project names
  # this url should still get the published repository, not ours.
  inputMap = {
    "https://ghc.gitlab.haskell.org/head.hackage/" = pkgs.haskell-nix.head-hackage-repo;
  };
}

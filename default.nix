{ system ? builtins.currentSystem
, sourcesOverride ? { }
, checkMaterialization ? false
, ...
}:

let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  flake-compat =
    with lock.nodes.flake-compat.locked;
    builtins.fetchTarball {
      url = "https://github.com/input-output-hk/flake-compat/archive/${rev}.tar.gz";
      sha256 = narHash;
    };
  self = import flake-compat {
    # We bypass flake-compat's rootSrc cleaning by evading its detection of this as a git
    # repo.
    # This is done for 3 reasons:
    # * To workaround https://github.com/edolstra/flake-compat/issues/25
    # * Make `updateMaterilized` scripts work (if filtering is done by `flake-compat`
    #   the `updateMaterilized` scripts will try to update the copy in the store).
    # * Allow more granular filtering done by the tests (the use of `cleanGit` and `cleanSourceWith`
    #   in `test/default.nix`).  If `flake-compat` copies the whole git repo, any change to the
    #   repo causes a change of input for all tests.
    src = { outPath = ./.; };
    override-inputs = sourcesOverride;
  };
in
self.defaultNix // (
  self.defaultNix.internal.compat {
    inherit system checkMaterialization;
  })

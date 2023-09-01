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
  self = (import flake-compat {
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
  }).defaultNix;

  inherit (self.inputs.nixpkgs) lib;

  # coming from internal.compat
  overlays = [ self.overlay ]
    ++ lib.optional checkMaterialization
    (final: prev: {
      haskell-nix = prev.haskell-nix // {
        checkMaterialization = true;
      };
    });
  nixpkgsArgs = {
    inherit overlays;
    inherit (self) config;
  };
  pkgs = import self.inputs.nixpkgs (nixpkgsArgs // {
    localSystem = { inherit system; };
  });
in
self // {
  inherit nixpkgsArgs pkgs;
  inherit (nixpkgsArgs) config overlays;
  sources = self.inputs;
  allOverlays = self.overlays;
  pkgs-2105 = import self.inputs.nixpkgs-2105 (nixpkgsArgs // {
    localSystem = { inherit system; };
  });
  pkgs-2111 = import self.inputs.nixpkgs-2111 (nixpkgsArgs // {
    localSystem = { inherit system; };
  });
  pkgs-2205 = import self.inputs.nixpkgs-2205 (nixpkgsArgs // {
    localSystem = { inherit system; };
  });
  pkgs-2211 = import self.inputs.nixpkgs-2211 (nixpkgsArgs // {
    localSystem = { inherit system; };
  });
  pkgs-2305 = import self.inputs.nixpkgs-2305 (nixpkgsArgs // {
    localSystem = { inherit system; };
  });
  pkgs-unstable = import self.inputs.nixpkgs-unstable (nixpkgsArgs // {
    localSystem = { inherit system; };
  });
  hix = import ./hix/default.nix { inherit pkgs; };
}

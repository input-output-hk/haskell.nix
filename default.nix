{ system ? builtins.currentSystem
, sourcesOverride ? { }
, checkMaterialization ? false
, ...
}:

let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);

  # NOTE: This has to be whitelisted in restricted evaluation mode
  flake-compat =
    with lock.nodes.flake-compat.locked;
    builtins.fetchTarball {
      url = "https://github.com/input-output-hk/flake-compat/archive/${rev}.tar.gz";
      sha256 = narHash;
    };

  # With flake-compat you will end up fetching the flake inputs with
  # builtins.fetchTarball. This is simply because you don't have access to any
  # nixpkgs before fetching the inputs.
  #
  # This won't work in restricted evaluation mode.
  #
  # Under the mild assumtion that https://github.com/NixOS is whitelisted, we
  # can manually fetch nixpkgs and let flake-compat fetch the rest of the
  # inputs with the nixpkgs just fetched.
  #
  # Manually fetch nixpkgs
  nixpkgs =
    with lock.nodes.nixpkgs.locked;
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      sha256 = narHash;
    };
  #
  # Instantiate the flake fetching the other inputs with the nixpkgs already
  # fetched
  self = (import flake-compat {
    pkgs = import nixpkgs { };
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
    (_final: prev: {
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

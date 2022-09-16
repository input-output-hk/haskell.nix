{...}@args:

let
  pins = (__fromJSON (__readFile ./flake.lock)).nodes;
  nixpkgsPin = pins.nixpkgs-2205.locked;
  flakeCompatPin = pins.flake-compat.locked;
  nixpkgsSrc =
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsPin.rev}.tar.gz";
      sha256 = nixpkgsPin.narHash;
    };
  pkgs = args.pkgs or (import nixpkgsSrc {});
  flake-compat =
    pkgs.fetchzip {
      url = "https://github.com/input-output-hk/flake-compat/archive/${flakeCompatPin.rev}.tar.gz";
      sha256 = flakeCompatPin.narHash;
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
    inherit pkgs;
  };
in self.defaultNix // (self.defaultNix.internal.compat
({ system = args.pkgs.system or builtins.currentSystem; } // args))

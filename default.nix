{...}@args:

let
  nixpkgsSrc =
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/110a2c9ebbf5d4a94486854f18a37a938cfacbbb.tar.gz";
      sha256 = "sha256-leWXLchbAbqOlLT6tju631G40SzQWPqaAXQG3zH1Imw=";
    };
  pkgs = args.pkgs or (import nixpkgsSrc {});
  flake-compat =
    pkgs.fetchzip {
      url = "https://github.com/hamishmack/flake-compat/archive/ce16b21b8a5588aa8b532353d3ceea89a38b8e77.tar.gz";
      sha256 = "sha256:054nsfqh3wy6v6bjamw0k91xl8v1rc5x2laic8mphrkrhzvyz5hi";
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

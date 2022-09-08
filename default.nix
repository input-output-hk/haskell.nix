{...}@args:

let
  nixpkgsSrc =
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/549d82bdd40f760a438c3c3497c1c61160f3de55.tar.gz";
      sha256 = "1rk9rh2ssp6zwqbnxa2cyncwjky9igkj90qdgbhzshwwxp89y6ai";
    };
  pkgs = args.pkgs or (import nixpkgsSrc {});
  flake-compat =
    pkgs.fetchzip {
      url = "https://github.com/edolstra/flake-compat/archive/5523c47f13259b981c49b26e28499724a5125fd8.tar.gz";
      sha256 = "sha256-7IySNHriQjzOZ88DDk6VDPf1GoUaOrOeUdukY62o52o=";
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

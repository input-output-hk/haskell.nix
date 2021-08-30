{...}@args:

let
  nixpkgsSrc = 
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/3c6f3f84af60a8ed5b8a79cf3026b7630fcdefb8.tar.gz";
      sha256 = "sha256:0jf9l6j60sa8cms7r4a02kr9j9884pwv1prf79b2ysnxmnhimnch";
    };
  pkgs = args.pkgs or (import nixpkgsSrc {});
  flake-compat =
    pkgs.fetchzip {
      url = "https://github.com/hamishmack/flake-compat/archive/ce16b21b8a5588aa8b532353d3ceea89a38b8e77.tar.gz";
      sha256 = "sha256:054nsfqh3wy6v6bjamw0k91xl8v1rc5x2laic8mphrkrhzvyz5hi";
    };
  self = import flake-compat {
    # This is a workaround for https://github.com/edolstra/flake-compat/issues/25:
    # If we're in pure-eval mode (signified by lack of builtins.currentSystem), then we
    # bypass flake-compat's rootSrc cleaning by evading its detection of this as a git
    # repo
    src = if builtins ? currentSystem then ./. else { outPath = ./.; };
    inherit pkgs;
  };
in self.defaultNix // (self.defaultNix.internal.compat
({ system = args.pkgs.system or builtins.currentSystem; } // args))

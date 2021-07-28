{...}@args:

let
  nixpkgsSrc = 
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/3c6f3f84af60a8ed5b8a79cf3026b7630fcdefb8.tar.gz";
      sha256 = "sha256:0jf9l6j60sa8cms7r4a02kr9j9884pwv1prf79b2ysnxmnhimnch";
    };
  pkgs = args.pkgs or import nixpkgsSrc {};
  flake-compat =
    pkgs.fetchgit {
      url = "https://github.com/hamishmack/flake-compat";
      rev = "90c3753102f9e619234b5d23f5db3b63f7bf8544";
      sha256 = "sha256-MM7nIIdFkA8hwG/vFqHZJ2+PP2CYgJifyPV3ET9TXyg=";
    };
  self = import flake-compat { src = ./.; inherit pkgs; };
in self.defaultNix.internal.compat
({ system = args.pkgs.system or builtins.currentSystem; } // args) // self.defaultNix

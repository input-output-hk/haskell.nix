{...}@args:

let
  nixpkgsSrc = 
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/3c6f3f84af60a8ed5b8a79cf3026b7630fcdefb8.tar.gz";
      sha256 = "sha256:0jf9l6j60sa8cms7r4a02kr9j9884pwv1prf79b2ysnxmnhimnch";
    };
  flake-compat =
    (args.pkgs or import nixpkgsSrc {}).fetchgit {
      url = "https://github.com/edolstra/flake-compat";
      rev = "99f1c2157fba4bfe6211a321fd0ee43199025dbf";
      sha256 = "sha256:0x2jn3vrawwv9xp15674wjz9pixwjyj3j771izayl962zziivbx2";
    };
  self = import flake-compat { src = ./.; };
in self.defaultNix.internal.compat
({ system = args.pkgs.system or builtins.currentSystem; } // args) // self.defaultNix

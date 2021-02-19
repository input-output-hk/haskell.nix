{...}@args:

let
  flake-compat =
    builtins.fetchGit { url = "https://github.com/edolstra/flake-compat"; };
  self = import flake-compat { src = ./.; };
in self.defaultNix.internal.compat
({ system = args.pkgs.system or builtins.currentSystem; } // args) // self.defaultNix

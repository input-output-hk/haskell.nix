with (import ./default.nix {});

{
  inherit nix-tools;

  tests = callPackage ./test {};
}

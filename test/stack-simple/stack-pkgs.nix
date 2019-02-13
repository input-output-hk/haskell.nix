{
  overlay = hackage:
    { packages = {} // { stack-simple = ./.stack.nix/stack-simple.nix; }; };
  resolver = "lts-13.6";
  }

let
  # You can use a tool like `niv` to manage this boilerplate
  hackageSrc = builtins.fetchTarball "https://github.com/input-output-hk/hackage.nix/archive/master.tar.gz";
  stackageSrc = builtins.fetchTarball "https://github.com/input-output-hk/stackage.nix/archive/master.tar.gz";
  haskellSrc = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz";

  haskellNix = import haskellSrc {
    # This allows you to override the pins used by `haskell.nix` internally
    sourcesOverride = {
      hackage = hackageSrc;
      stackage = stackageSrc;
    };
  };
in {
  inherit haskellNix
  # ...
}
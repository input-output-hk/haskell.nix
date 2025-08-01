{
  lib,
  haskell-nix,
}:

haskell-nix.cabalProject' {
  name = "make-install-plan";
  src = ./make-install-plan;

  compiler-nix-name = lib.mkDefault "ghc984";

  # tests need to fetch hackage
  configureArgs = lib.mkDefault "--disable-tests";

  # Tools to include in the development shell
  shell.tools = {
    cabal = "latest";
    haskell-language-server.src = "latest";
  };
}

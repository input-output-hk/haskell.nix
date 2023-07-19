{pkgs, config, ...}@projectArgs: {
  name = "nix-tools";
  materialized = ../../materialized + "/${config.compiler-nix-name}/nix-tools";

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
 }

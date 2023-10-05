{ sources }:
# This hydra overlay is required to circumvent problems with builtins.path
# in restricted eval mode.
# This can be removed once hydra in nixpkgs is based on a recent enough nix,
# which contains this fix: https://github.com/NixOS/nix/pull/5163

_final: prev: {
  hydra-unstable = sources.hydra.defaultPackage.${prev.system};
}

{ lib, config, pkgs, ... }:

let
  # it also crucially depends on system, and compiler, both of which need to be resolved to the
  # current system being targeted.
  hostMap = import ../lib/host-map.nix pkgs.stdenv;
  cabal = import ../lib/cabal-os-arch-comp.nix;

  compiler = cabal.compiler // {
    isGhc = true;
    # this is partially a hack to support `impl(ghcjs)`.
    # We set GHC == true _and_ GHCJS == true.
    isGhcjs = hostMap.os == "Ghcjs";
    # maybe we need something for asterius here
    # as well.
    version = lib.mapAttrs (_: f: v: f (builtins.compareVersions config.compiler.version v)) {
      eq = c: c == 0;
      gt = c: c > 0;
      ge = c: c >= 0;
      lt = c: c < 0;
      le = c: c <= 0;
    };
  };
  system = cabal.os   // { "is${hostMap.os}"   = true; }
        // cabal.arch // { "is${hostMap.arch}" = true; };

in {
  options.cabal.compiler = lib.mkOption {
    type = lib.types.unspecified;
  };
  options.cabal.system = lib.mkOption {
    type = lib.types.unspecified;
  };
  config.cabal = { inherit compiler system; };
}

# TODO remove this file when we no longer need to build with the
# non flake hydra configuration.
{
  supportedSystems ? ["x86_64-linux" "x86_64-darwin"]
, include ? (compiler-nix-name: true)
}: let
  defaultNix = import ./. {};

  inherit (defaultNix) pkgs;
  inherit (pkgs) lib;

  filterCiJobs = __mapAttrs (systemName: system:
    lib.optionalAttrs (__elem systemName supportedSystems) (
      __mapAttrs (n: jobs:
        let ghcMatch = __match ".*-(ghc[0-9]*)-.*" n;
        in
          lib.optionalAttrs (
            (n == "latest" && include "ghc8107")
            || (ghcMatch != null && include (__head ghcMatch))) jobs) system));

  jobs = lib.getAttrs supportedSystems (filterCiJobs defaultNix.ciJobs);

  required = defaultNix.pkgs.releaseTools.aggregate {
    name = "github-required";
    meta.description = "All jobs required to pass CI";
    constituents = lib.collect lib.isDerivation jobs;
  };
in
  jobs // { inherit required; }
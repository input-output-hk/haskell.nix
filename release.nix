# TODO remove this file when we no longer need to build with the
# non flake hydra configuration.
{
  supportedSystems ? ["x86_64-linux" "x86_64-darwin"],
}: let
  defaultNix = import ./. {};

  inherit (defaultNix) pkgs;
  inherit (pkgs) lib;

  jobs = lib.getAttrs supportedSystems defaultNix.ciJobs;

  required = defaultNix.pkgs.releaseTools.aggregate {
    name = "github-required";
    meta.description = "All jobs required to pass CI";
    constituents = lib.collect lib.isDerivation jobs;
  };
in
  jobs // { inherit required; }
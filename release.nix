# TODO remove this file when we no longer need to build with the
# non flake hydra configuration.
{
  supportedSystems ? ["x86_64-linux" "x86_64-darwin"]
, include ? (compiler-nix-name: true)
}:
let
  traceNames = prefix: builtins.mapAttrs (n: v:
    if builtins.isAttrs v
      then if v ? type && v.type == "derivation"
        then __trace (prefix + n) v
        else traceNames (prefix + n + ".") v
      else v);

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

  jobs = lib.getAttrs supportedSystems (filterCiJobs defaultNix.hydraJobs);

  windows-secp256k1 =
    let
      pkgs = (import ./. {}).pkgs-unstable; 
      makeBinDist = drv: pkgs.runCommand drv.name {
        nativeBuildInputs = [ pkgs.zip ];
      } ''
        mkdir -p $out/nix-support
        cp -r ${drv}/* .
        chmod -R +w .
        zip -r $out/${drv.name}.zip .
        echo "file binary-dist $out/${drv.name}.zip" > $out/nix-support/hydra-build-products
      '';
    in makeBinDist pkgs.pkgsCross.mingwW64.secp256k1;
  # This job causes in eval if we include everything.
  # For now just including some darwin checks (since thos are not done on cicero)
  required = defaultNix.pkgs.releaseTools.aggregate {
    name = "github-required";
    meta.description = "All jobs required to pass CI";
    constituents = lib.collect lib.isDerivation (
      lib.optionalAttrs (jobs ? x86_64-darwin) {
        darwin-ghc8107  = jobs.x86_64-darwin.required-unstable-ghc8107-native;
        darwin-ghc927   = jobs.x86_64-darwin.required-unstable-ghc927-native;
      }
    );
  };
in
  traceNames "job " (jobs // { inherit windows-secp256k1 required; })

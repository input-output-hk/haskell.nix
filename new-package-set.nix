{ pkgs ? import <nixpkgs> {}
, planFunc ? import ../stackage/lts-11.4.nix
, hackage ? import ../hackage
, driver ? ./nix/component-driver.nix
}:

(pkgs.lib.fix (self: with self; {
  inherit (pkgs) lib stdenv;
  ghc = pkgs.haskell.compiler.${plan.compiler.nix-name};

  # Avoid pkgs.callPackage for now. It does a lot of nonsense with OOP
  # style programming that we should avoid until we know we want it.
  weakCallPackage = scope: f: args:
    let f' = if lib.isFunction f then f else import f;
        args' = scope // args;
    in f' (builtins.intersectAttrs (builtins.functionArgs f') args');

  haskellLib = import ./lib { inherit lib haskellLib; };

  hackageConfigs =
    lib.flip lib.mapAttrs hackage
      (pname: lib.mapAttrs
        (vnum: version: version // {
          revisions =
            let
              rev2Config = rev: {
                inherit (version) sha256;
                inherit (rev) outPath;
                revision = rev.revNum;
                revisionSha256 = rev.sha256;
              };
              f = rev: acc: acc // {
                # If there's a collision (e.g. a revision was
                # reverted), pick the one with the smaller
                # revNum. They're identical, but if the smaller one is
                # r0 then we don't have to download a cabal file.
                ${rev.sha256} = if lib.hasAttr rev.sha256 acc && acc.${rev.sha256}.revNum < rev.revNum
                  then acc.${rev.sha256}
                  else rev;
              };
              contentAddressedRevs = lib.foldr f {} (builtins.attrValues version.revisions);
            in lib.mapAttrs (_: rev2Config) (version.revisions // contentAddressedRevs);
        }));
  plan =
    let p = planFunc hackageConfigs;
    in p // {
      packages = hsPkgs:
        let
          args = {
            inherit hsPkgs compiler system pkgconfPkgs;
            pkgs = adjustedPkgs;
          };
        in lib.mapAttrs (pname: { revision, flags ? {} }: self:
          # Set the flags with the rhs of the recursiveUpdate, but
          # pass the final choice in flags using open recursion.
          lib.recursiveUpdate (import revision (args // { inherit (self) flags; })) {
            inherit flags;
            inherit (revision) sha256 revision revisionSha256;
          }
        )
        p.packages;
    };

  cabal = import ./lib/cabal-os-arch-comp.nix;

  compiler = cabal.compiler // {
    isGhc = true;
    version = lib.mapAttrs (_: f: v: f (builtins.compareVersions plan.compiler.version v)) {
      eq = c: c == 0;
      gt = c: c > 0;
      ge = c: c >= 0;
      lt = c: c < 0;
      le = c: c <= 0;
    };
  };
  system = let
    hostMap = import ./lib/host-map.nix pkgs.stdenv;
  in cabal.os // { "is${hostMap.os}" = true; }
    // cabal.arch // { "is${hostMap.arch}" = true; };

  adjustedPkgs = pkgs // (import ./lib/system-nixpkgs-map.nix pkgs);
  pkgconfPkgs = pkgs // (import ./lib/pkgsconf-nixpkgs-map.nix pkgs);
  # hsPkgs = adjustedPkgs
  #   // { buildPackages = hsPkgs; }
  #   // lib.mapAttrs (_: _: null) (plan.compiler.packages // { hsc2hs = "0.68.2"; })
  #   // lib.mapAttrs (_: driver) configs;

  hsPkgs = weakCallPackage pkgs driver {
    inherit haskellLib ghc weakCallPackage plan;
  };
}))

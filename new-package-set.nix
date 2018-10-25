{ pkgs ? import <nixpkgs> {}
, planFunc ? import ../stackage/lts-11.4.nix
, hackage ? import ../hackage
, driver ? ./nix/component-driver.nix
}:

{ extraDeps ? hsPkgs: {} }:
(pkgs.lib.fix (self: with self; {
  inherit (pkgs) lib stdenv;

  # obtain the compiler from the haskell packages.
  # this should allow us to use `config` overrides
  # in the nixpkgs setup, and properly override the
  # complier as needed.
  ghc = pkgs.haskell.compiler.${plan.compiler.nix-name};

  # Avoid pkgs.callPackage for now. It does a lot of nonsense with OOP
  # style programming that we should avoid until we know we want it.

  # weakCallPackage: call a function or (importable expression)
  # with scope + args.
  #
  # weakCallPackage scope f args
  #  will call f (scope // args)
  #
  # weakCallpackage scope ./path args
  #  will call the expression at ./path with (scope // args)
  #
  weakCallPackage = scope: f: args:
    let f' = if lib.isFunction f then f else import f;
        args' = scope // args;
    in f' (builtins.intersectAttrs (builtins.functionArgs f') args');

  # this is *not* the hasekllLib from nixpkgs; it is rather our own
  # library from haskell.nix
  haskellLib = import ./lib { inherit lib haskellLib; };

  # hackage looks like the following:
  # { "package-name" =
  #   { "a.b.c.d" =
  #     rec { sha256 = $pkgVersionSha256;
  #           revisions =
  #           { r0 = { outPath = ./hackage/...; revNum = 0; sha256 = $revisionSha256; };
  #             default = revisions.r0; };
  #         };
  #     };
  #   };
  # }


  # However it's more convenient to deal with the leaf nodes, and as such
  # push the `sha256` of the package/version into the revision and keep the
  # revision.sha256 as revisionSha256; as well as making the revision content
  # addressable by it's revision hash.
  #
  # Thus we transform hackage into hackageConfigs, which will look like:
  # { "package-name" =
  #   { "a.b.c.d" =
  #     rec { sha256 = $packageVersionSha256;
  #           revisions =
  #           { r0 = { outPath = ./hackage/...;
  #                    sha256 = $packageVersionSha256;
  #                    revision = $revNum;
  #                    revisionSha256 = $revisionSha256; };
  #             default = revisions.r0;
  #             $revisionSha256 = revisions.r0; };
  #         };
  #     };
  #   };
  # }

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

  # The planFunc (that is, a package set description like an LTS set
  # or a plan.nix (derived from plan.json)) will then producde a structure
  # that looks like:
  #
  # { packages = { "package" = { revision = hackageConfigs.$package.$version.revisions.default;
  #                              flags = { flag1 = true; flag2 = false; ... }; };
  #                ... };
  #   compiler = { version = "X.Y.Z"; nix-name ="ghcXYZ";
  #                # packages that come bundled with the compiler
  #                packages = { "bytestring" = "a.b.c.d"; ... }; };
  # }

  # package descriptions in hackage will look like:
  # { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs }:
  # { flags = { flag1 = false; flags2 = true; ... };
  #   package = { specVersion = "X.Y"; identifier = { name = "..."; version = "a.b.c.d"; };
  #               license = "..."; copyright = "..."; maintainer = "..."; author = "...";
  #               homepage = "..."; url = "..."; synopsis = "..."; description = "...";
  #               buildType = "Simple"; # or Custom, Autoconf, ...
  #             };
  #  components = {
  #    "..." = { depends = [ (hsPkgs.base) ... ]; };
  #    exes = { "..." = { depends = ... };
  #             "..." = { depends = ... }; };
  #    tests = { "..." = { depends = ... }; ... };
  #  };

  # The package descriptions depend on pkgs, which are used to resolve system package dependencies
  # as well as pkgconfPkgs, which are used to resolve pkgconfig name to nixpkgs names.  We simply
  # augment the existing pkgs set with the specific mappings:
  adjustedPkgs = pkgs // (import ./lib/system-nixpkgs-map.nix pkgs);
  pkgconfPkgs = pkgs // (import ./lib/pkgconf-nixpkgs-map.nix pkgs);

  # it also crucially depends on system, and compiler, both of which need to be resolved to the
  # current system being targetted.
  hostMap = import ./lib/host-map.nix pkgs.stdenv;
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
  system = cabal.os   // { "is${hostMap.os}"   = true; }
        // cabal.arch // { "is${hostMap.arch}" = true; };

  # Therefore the arguments we pass to the package are
  pkgArgs = { inherit compiler system pkgconfPkgs; pkgs = adjustedPkgs; };

  # we can never reinstall the following set of packages:
  nonReinstallables
    = [ "rts" "ghc" "base" "ghc-prim" "array" "integer-gmp" "integer-simple"
        "deepseq" "pretty" "ghc-boot-th" "template-haskell" "terminfo" ];
  # Thus the final plan will contain
  # { packages = hsPkgs: { "package" = { flags = ...; package = ...; components = ...; }; ...  };
  #   compiler = ... };

  plan =
    let p = planFunc hackageConfigs;
    in p // {
      packages = hsPkgs:
        (lib.mapAttrs (pname: { revision, flags ? {} }: self:
          # Set the flags with the rhs of the recursiveUpdate, but
          # pass the final choice in flags using open recursion
          # (provided with a refernece to self).
          #
          # NB: we can import revision, because revision.outPath is
          #     an importable path. Magic!
          lib.recursiveUpdate (import revision (pkgArgs // { inherit hsPkgs; inherit (self) flags; })) {
            inherit flags;
            inherit (revision) sha256 revision revisionSha256;
          }
        )
        p.packages)
        // (lib.mapAttrs (pname: path: self:
             import path (pkgArgs // { inherit hsPkgs; inherit (self) flags; })
           )
           extraDeps)
        // (builtins.foldl' (acc: x: acc // { "${x}" = null; }) {} nonReinstallables)
        ;
    };

  hsPkgs = weakCallPackage pkgs driver {
    inherit haskellLib ghc weakCallPackage plan;
  };
}))

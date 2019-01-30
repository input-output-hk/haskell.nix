{ lib, config, pkgs, ... }:

let
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
    lib.flip lib.mapAttrs config.hackage.db
      (pname: lib.mapAttrs
        (vnum: version: version // {
          revisions =
            let
              rev2Config = rev: { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }@modArgs: {
                inherit (version) sha256;
                revision = rev.revNum;
                revisionSha256 = rev.sha256;
              } // import rev modArgs;
              f = rev: {
                name = rev.sha256;
                value = rev;
              };
              sortedRevisions = builtins.sort (a: b: a.revNum < b.revNum) (builtins.attrValues version.revisions);
              # if you give listToAttrs duplicate keys, the 1st one wins (but
              # the internal array for the set will be larger then it needs to
              # be).  This implicitly means if there were collisions in the
              # revsisions (e.g. a version was reverted), we pick the one with
              # the samller revNum.  They are identical, but if the smaller one
              # is r0 then we don't have to download a cabal file.
              contentAddressedRevs = builtins.listToAttrs (map f sortedRevisions);
            in lib.mapAttrs (_: rev2Config) (version.revisions // contentAddressedRevs);
        }));

in {
  options.ghc.package = lib.mkOption {
    type = lib.types.package;
    # obtain the compiler from the haskell packages.
    # this should allow us to use `config` overrides
    # in the nixpkgs setup, and properly override the
    # complier as needed.
    default = pkgs.buildPackages.haskell.compiler.${config.compiler.nix-name};
    defaultText = "pkgs.buildPackages.haskell.compiler.\${config.compiler.nix-name}";
  };

  options.hackage.db = lib.mkOption {
    type = lib.types.unspecified; # TODO: Worth type checking with modules? Or does that slow eval too much?
  };
  options.hackage.configs = lib.mkOption {
    type = lib.types.unspecified;
  };
  # Note: we inject rts."1.0" as invalid here. This package can only
  # be built by GHC's build system.  However it may show up in stackage
  # snapshots. As such we just null it out.
  config.hackage.configs = hackageConfigs
                        // { rts."1.0".revisions.default = null; };
}

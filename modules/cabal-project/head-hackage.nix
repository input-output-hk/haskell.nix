{ lib, config, pkgs, ... }:
let
  cfg = config.head-hackage;
in
{
  _file = "haskell.nix/modules/cabal-project/head-hackage.nix";

  options = {
    head-hackage = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default =
          builtins.compareVersions
            pkgs.buildPackages.haskell-nix.compiler.${config.compiler-nix-name}.version
            "9.8.0" >= 0;
        defaultText = lib.literalExpression ''
          builtins.compareVersions
            pkgs.buildPackages.haskell-nix.compiler.''${config.compiler-nix-name}.version
            "9.8.0" >= 0;
        '';
        example = true;
        description = lib.mdDoc ''
          When building ghc 9.8 and ghc HEAD projects we need to include the
          `head.hackage` repository to get the patched versions of packages that
          are needed for those versions of GHC.
        '';
      };

      sha256 = lib.mkOption {
        type = lib.types.str;
        default = "sha256-DXv6ZLGdD17ppJdww7NUYdKnKtEAMOIayvK/hO4+DL8=";
        description = lib.mdDoc ''
          Currently the sha256 needs regular updating as there is no way to pin
          `head.hackage`.
        '';
      };

      allow-newer = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = lib.mdDoc ''
          Whether or not include "allow-newer: all" in the cabal project config
          file. True by default as it is often needed when working with new
          compiler versions.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    repositories = [{
      name = "head.hackage.ghc.haskell.org";
      url = "https://ghc.gitlab.haskell.org/head.hackage/";
      secure = true;
      key-threshold = 3;
      root-keys = [
        "f76d08be13e9a61a377a85e2fb63f4c5435d40f8feb3e12eb05905edb8cdea89"
        "26021a13b401500c8eb2761ca95c61f2d625bfef951b939a8124ed12ecf07329"
        "7541f32a4ccca4f97aea3b22f5e593ba2c0267546016b992dfadcd2fe944e55d"
      ];
      sha256 = cfg.sha256;
      override = true;
    }];

    # TODO: this really should set cabalProject but the logic around
    # missing/implicit cabal.project files is fragile. Revisit this in the
    # future.
    cabalProjectLocal = lib.mkIf cfg.allow-newer "allow-newer: all";
  };
}

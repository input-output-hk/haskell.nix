{ projectConfig }:
{ lib, config, pkgs, haskellLib, ... }: {
  options = {
    packages = lib.mkOption {
      type = lib.types.unspecified;
      default = haskellLib.selectProjectPackages;
    };
    crossPlatforms = lib.mkOption {
      type = lib.types.unspecified;
      default = projectConfig.crossPlatforms;
    };
    variants = lib.mkOption {
      type = lib.types.attrsOf lib.types.unspecified;
      default = {};
      description = ''
        This allows flakes to easily include variations of the
        project by with different project arguments.
        Anything you can pass to `project.addModule` can be used.
        For instance to include variants using ghc 9.2.4:
        ```
          flake.variants.ghc924.compiler-nix-name = pkgs.lib.mkForce "ghc924";
        ```
        Then use it with:
        ```
          nix build .#ghc924:hello:exe:hello
        ```
      '';
    };
    coverage = lib.mkOption {
      type = lib.types.unspecified;
      default = {};
      description = ''
        Project module for use when generating coverage reports.
        The project packages will have `doCoverage` by default.
      '';
    };
  };
}

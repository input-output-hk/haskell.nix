{ config, lib, ... }:

with lib;

let

  repositoryModule = types.submodule ({ config, ... }: {
    options = {
      name = mkOption {
        type = types.str;
        description = "The repository name.";
      };

      src = mkOption {
        type = types.package;
        description = "Path to the repository's content.";
      };

      isSecure = mkOption {
        type = types.bool;
        description = "Whether the repository uses hackage-security's format.";
        default = true;
      };

      cabalProject = mkOption {
        type = types.str;
        description = "The cabal project configuration to be added to cabal.project";
      };
    };

    config.cabalProject =
      ''
        repository ${config.name}
          url: file:${config.src}
      '' + lib.optionalString config.isSecure "  secure: True";
  });

in

{
  _file = "haskell.nix/modules/cabal-project/repository.nix";

  options.repository = mkOption {
    type = with types; listOf (oneOf [ str repositoryModule ]);
    default = [ ];
    example = literalExpression ''
      [
        {
          name = "cardano-haskell-packages";
          src = fetchFromGitHub {
            owner = "input-output-hk";
            repo = "cardano-haskell-packages";
            rev = "3de0669a067675b42cc7539d0e0a87a163738dbf";
            hash = "sha256-mlMYoWakSExTzt1GIueIQIRfSLNbSeOj65vxPz3Vw5E=";
          };
        }
      ]
    '';
  };

  config.cabalProjectLocal =
    mkMerge (builtins.map
      (repo: if repo ? cabalProject then repo.cabalProject else repo)
      config.repository);
}

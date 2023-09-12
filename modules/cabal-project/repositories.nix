{ config, lib, ... }:
with lib;
let
  repository = with types;
    submodule {
      options = {
        name = mkOption { type = str; };
        url = mkOption { type = str; };
        secure = mkOption { type = bool; default = true; };
        root-keys = mkOption { type = nullOr (listOf str); default = null; };
        key-threshold = mkOption { type = nullOr int; default = null; };
        sha256 = mkOption { type = nullOr str; default = null; };
        override = mkOption { type = bool; default = false; };
      };
    };

  renderRepository = repo:
    ''
      repository ${repo.name}
        url: ${repo.url}
        secure: ${ if repo.secure then "True" else "False" }
    '' +
    optionalString (repo.root-keys != null) (
      "  root-keys:\n" +
      concatMapStringsSep "\n" (str: "    ${str}") repo.root-keys +
      "\n"
    ) +
    optionalString (repo.key-threshold != null)
      "  key-threshold: ${builtins.toString repo.key-threshold}\n"
    +
    optionalString (repo.sha256 != null)
      "  --sha256: ${repo.sha256}\n";
in
{
  _file = "haskell.nix/modules/cabal-project/repositories.nix";

  options = {
    repositories = mkOption {
      type = with types; listOf (either str repository);
      default = [ ];
      description = lib.mdDoc "Additional repository specifications";
      example = [
        {
          name = "cardano-haskell-packages";
          url = "https://input-output-hk.github.io/cardano-haskell-packages";
          secure = true;
        }
        ''
          repository ghcjs-overlay
            url: https://raw.githubusercontent.com/input-output-hk/hackage-overlay-ghcjs/91f4ce9bea0e7f739b7495647c3f72a308ed1c6f
            secure: True
            root-keys:
            key-threshold: 0
            --sha256: sha256-mZT7c+xR5cUTjLdCqOxpprjYL3kr/+9rmumtXvWAQlM=
        ''
      ];
    };

    active-repositories = mkOption {
      type = with types; listOf str;
      description = "Specify active package repositories";
    };
  };

  config = {
    # TODO: this really should set cabalProject but the logic around
    # missing/implicit cabal.project files is fragile. Revisit this in the
    # future.
    #
    # Using mkMerge here and not directly concatStringsSep to avoid inserting an
    # empty line into the configuration file.
    cabalProjectLocal = lib.mkMerge (
      (builtins.map
        (repo: if isString repo then repo else renderRepository repo)
        config.repositories)
      ++
      # Only :rest is the default value so we can avoid adding it to the
      # configuration if it is the only value we have.
      (lib.optional (config.active-repositories != [ ":rest" ])
        "active-repositories: ${lib.concatStringsSep ", " config.active-repositories}")
    );

    active-repositories = lib.mkMerge [
      [ ":rest" ]
      (builtins.map
        (repo: repo.name + lib.optionalString repo.override ":override")
        config.repositories)
    ];
  };
}

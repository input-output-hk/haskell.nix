{
  pkgs = hackage:
    {
      packages = {
        ghc-prim.revision = hackage.ghc-prim."0.6.1".revisions.default;
        base.revision = hackage.base."4.14.3.0".revisions.default;
        deepseq.revision = hackage.deepseq."1.4.4.0".revisions.default;
        integer-gmp.revision = hackage.integer-gmp."1.0.3.0".revisions.default;
        containers.revision = hackage.containers."0.6.5.1".revisions.default;
        array.revision = hackage.array."0.5.4.0".revisions.default;
      };
      compiler = {
        version = "8.10.7";
        nix-name = "ghc8107";
        packages = {
          "containers" = "0.6.5.1";
          "ghc-prim" = "0.6.1";
          "base" = "4.14.3.0";
          "integer-gmp" = "1.0.3.0";
          "deepseq" = "1.4.4.0";
          "array" = "0.5.4.0";
        };
      };
    };
  extras = hackage:
    { packages = { hscolour = ./.plan.nix/hscolour.nix; }; };
  modules = [
    {
      preExistingPkgs = [
        "ghc-prim"
        "base"
        "deepseq"
        "integer-gmp"
        "containers"
        "array"
      ];
    }
    ({ lib, ... }:
      { packages = { "hscolour" = { flags = {}; }; }; })
    ({ lib, ... }:
      {
        packages = {
          "hscolour".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "hscolour".components.exes."HsColour".planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
        };
      })
  ];
}
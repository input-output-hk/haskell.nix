{ config, pkgs, lib, haskellLib, buildModules, ... }:
let
  builder = haskellLib.weakCallPackage pkgs ../builder {
    inherit haskellLib;
    ghc = config.ghc.package;
    compiler-nix-name = config.compiler.nix-name;
    inherit (config) nonReinstallablePkgs hsPkgs compiler evalPackages;
  };

in

{
  # Packages in that are `pre-existing` in the cabal plan
  options.preExistingPkgs = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [];
  };
  # This has a slightly modified option type. We will *overwrite* any previous
  # setting of nonRelocatablePkgs, instead of merging them.  Otherwise you
  # have no chance of removing packages retroactively.  We might improve this
  # by implementing a logic that would allow +xxx to be added, -xxx to be removed
  # and if it's not a list of -/+ prefixed strings, be assumed to be overwriting.
  # This seems ugly.
  options.nonReinstallablePkgs = lib.mkOption {
    type = (lib.types.listOf lib.types.str) // {
      merge = _loc: defs: lib.last (lib.getValues defs);
    };
  };

  options.reinstallableLibGhc = lib.mkOption {
    type = lib.types.bool;
    default = !pkgs.stdenv.hostPlatform.isGhcjs;
    description = "Is lib:ghc reinstallable?";
  };
  options.setup-depends = lib.mkOption {
    type = lib.types.listOf lib.types.unspecified;
    default = [];
    description = "pkgs to globally provide to Setup.hs builds";
  };

  # Dependencies (with reinstallable-lib:ghc)
  #
  #              .--------.           .------------------.
  #              | pretty | < ------- | template-haskell |
  #              '--------'           '------------------'
  #                   v                          |
  #              .---------.     .-------.       |
  #              | deepseq | - > | array |       |
  #              '---------'     '-------'       v
  #                    v            v         .-------------.
  # .----------.  .----------.  .------.   .- | ghc-boot-th |
  # | ghc-heap |  | ghc-prim |  | base |< -'  '-------------'
  # '----------'  '----------'  '------'  .----------------.
  #       |          v           |  |     | integer-simple |
  #       |       .-----.        |  '-- > |-------or-------|
  #       '---- > | rts | < -----'        | integer-gmp    |
  #               '-----'                 '----------------'
  #
  # without reinstallable-lib:ghc, this is significantly larger.

  config.nonReinstallablePkgs = if config.preExistingPkgs != []
   then ["rts"] ++ config.preExistingPkgs
    ++ lib.optionals (builtins.compareVersions config.compiler.version "8.11" < 0 && pkgs.stdenv.hostPlatform.isGhcjs) [
      "ghcjs-prim" "ghcjs-th"]
   else
    [ "rts" "ghc-prim" "integer-gmp" "integer-simple" "base"
      "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
      # ghcjs custom packages
      "ghcjs-prim" "ghcjs-th"
    ]
    ++ lib.optionals (builtins.compareVersions config.compiler.version "8.11" >= 0) [
      "ghc-bignum"]
    ++ lib.optionals (builtins.compareVersions config.compiler.version "9.1" >= 0) [
      "system-cxx-std-lib"]
    ++ lib.optionals (builtins.compareVersions config.compiler.version "9.9" >= 0) [
      "ghc-internal"]
    ++ lib.optionals (!config.reinstallableLibGhc) ([
      "ghc-boot"
      "ghc" "Cabal" "Win32" "array" "binary" "bytestring" "containers"
      "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
      # "ghci" "haskeline"
      "hpc"
      "mtl" "parsec" "process" "text" "time" "transformers"
      "unix" "xhtml" "terminfo"
    ]
    ++ lib.optionals (builtins.compareVersions config.compiler.version "8.11" >= 0) [
      # stm and exceptions are needed by the GHC package since 9.0.1
      "stm" "exceptions"]
    ++ lib.optionals (builtins.compareVersions config.compiler.version "9.8.1" >= 0) [
      "semaphore-compat"]
    ++ lib.optionals (builtins.compareVersions config.compiler.version "9.9" >= 0) [
      "os-string"]
    )
    ++ lib.optionals (!config.reinstallableLibGhc || __elem config.compiler.nix-name ["ghc865"]) [
      "ghc-heap"
    ];

  options.bootPkgs = lib.mkOption {
    type = lib.types.listOf lib.types.str;
  };

  config.bootPkgs =  [
      "rts" "ghc-boot-th"
      "ghcjs-prim"
   ] ++ lib.optional (!config.reinstallableLibGhc) "ghc"
    ++ lib.optionals (
      !__elem config.compiler.nix-name ["ghc865" "ghc881" "ghc882" "ghc883" "ghc884" "ghc8101" "ghc8102" "ghc8103" "ghc8104" "ghc8105" "ghc8106" "ghc8107"]) [
      "ghc-bignum" ]
    ++ lib.optionals (
      !__elem config.compiler.nix-name ["ghc865" "ghc881" "ghc882" "ghc883" "ghc884" "ghc8101" "ghc8102" "ghc8103" "ghc8104" "ghc8105" "ghc8106" "ghc8107" "ghc901" "ghc902"]) [
      "system-cxx-std-lib" ];

  options.hsPkgs = lib.mkOption {
    type = lib.types.unspecified;
  };

  config.hsPkgs =
    { inherit (builder) shellFor makeConfigFiles ghcWithPackages ghcWithHoogle;
      buildPackages = buildModules.config.hsPkgs; # TODO perhaps remove this
      pkgsBuildBuild = buildModules.config.hsPkgs;
    } //
    lib.mapAttrs
      (_name: pkg: if pkg == null then null else builder.build-package config pkg)
      (config.packages // lib.genAttrs (config.nonReinstallablePkgs ++ config.bootPkgs) (_: null));
}

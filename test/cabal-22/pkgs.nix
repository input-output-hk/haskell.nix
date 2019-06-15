{
  pkgs = hackage:
    {
      packages = {
        "tf-random".revision = (((hackage."tf-random")."0.5").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.2.0").revisions).default;
        "stm".revision = (((hackage."stm")."2.4.5.1").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "clock".revision = (((hackage."clock")."0.8").revisions).default;
        "clock".flags.llvm = false;
        "QuickCheck".revision = (((hackage."QuickCheck")."2.13.1").revisions).default;
        "QuickCheck".flags.templatehaskell = true;
        "hspec-discover".revision = (((hackage."hspec-discover")."2.7.1").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.3.0").revisions).default;
        "random".revision = (((hackage."random")."1.1").revisions).default;
        "splitmix".revision = (((hackage."splitmix")."0.0.2").revisions).default;
        "HUnit".revision = (((hackage."HUnit")."1.6.0.0").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.1.5").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.13.0.0").revisions).default;
        "hspec-expectations".revision = (((hackage."hspec-expectations")."0.8.2").revisions).default;
        "call-stack".revision = (((hackage."call-stack")."0.1.0").revisions).default;
        "primitive".revision = (((hackage."primitive")."0.7.0.0").revisions).default;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.9.1").revisions).default;
        "ansi-terminal".flags.example = false;
        "containers".revision = (((hackage."containers")."0.5.11.0").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.8.2").revisions).default;
        "setenv".revision = (((hackage."setenv")."0.1.1.3").revisions).default;
        "base".revision = (((hackage."base")."4.11.1.0").revisions).default;
        "hspec".revision = (((hackage."hspec")."2.7.1").revisions).default;
        "time".revision = (((hackage."time")."1.8.0.2").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "quickcheck-io".revision = (((hackage."quickcheck-io")."0.2.0").revisions).default;
        "colour".revision = (((hackage."colour")."2.3.5").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2").revisions).default;
        "hspec-core".revision = (((hackage."hspec-core")."2.7.1").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.4.4").revisions).default;
        "array".revision = (((hackage."array")."0.5.2.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.4.4";
        nix-name = "ghc844";
        packages = {
          "ghc-prim" = "0.5.2.0";
          "stm" = "2.4.5.1";
          "unix" = "2.7.2.2";
          "rts" = "1.0";
          "deepseq" = "1.4.3.0";
          "directory" = "1.3.1.5";
          "template-haskell" = "2.13.0.0";
          "containers" = "0.5.11.0";
          "bytestring" = "0.10.8.2";
          "base" = "4.11.1.0";
          "time" = "1.8.0.2";
          "filepath" = "1.4.2";
          "pretty" = "1.1.3.6";
          "ghc-boot-th" = "8.4.4";
          "array" = "0.5.2.0";
          "integer-gmp" = "1.0.2.0";
          };
        };
      };
  extras = hackage:
    { packages = { project = ./.plan.nix/project.nix; }; };
  }
{
  extras = hackage:
    {
      packages = {
        "transformers" = (((hackage.transformers)."0.5.6.2").revisions).default;
        "process" = (((hackage.process)."1.6.5.0").revisions).default;
        stack-simple = ./stack-simple.nix;
        };
      };
  resolver = "lts-13.26";
  modules = [ { packages = {}; } ];
  }
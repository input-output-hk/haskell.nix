# shell.nix
let
  project = import ./default.nix;
in
  project.shellFor {
    # ALL of these arguments are optional.

    # List of packages from the project you want to work on in
    # the shell (default is all the projects local packages).
    packages = ps: with ps; [
      pkga
      pkgb
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = true;

    # Some common tools can be added with the `tools` argument
    tools = {
      cabal = "3.2.0.0";
      hlint = "latest"; # Selects the latest version in the hackage.nix snapshot
      haskell-language-server = "latest";
    };
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
    buildInputs = [ (import <nixpkgs> {}).git ];

    # Sellect cross compilers to include.
    crossPlatforms = ps: with ps; [
      ghcjs      # Adds support for `js-unknown-ghcjs-cabal build` in the shell
      # mingwW64 # Adds support for `x86_64-W64-mingw32-cabal build` in the shell
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
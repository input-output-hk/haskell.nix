{ sources }:
let
  overlays = {
    wine = import ./wine.nix;
    haskell = import ./haskell.nix { inherit sources; };

    # Here is where we import nix-tools into the overlays that haskell.nix is
    # going to use. To cut the evaluation time of nix-tools (which would itself
    # depend on haskell.nix), we have the option of obtaining a pre-compiled
    # and statically-linked copy nix-tools.
    nix-tools = (final: prev:
      let
        # Import the overlay from nix-tools' subdirectory
        nix-tools-pkgs = import ../nix-tools/overlay.nix final prev;

        # The static-nix-tools tarball.
        #
        # Note: nix-tools provides single derivations for each of the tools it
        # provides, and haskell.nix derivations are granual in which tools they
        # are going to need. E.g. a derivation will have
        #     nativeBuildInputs = [ nix-tools.exes.make-install-plan ... ];
        #
        # On the other hand, there is a single the binary tarball for all
        # tools, therefore we cannot just swap nix-tools for a derivation
        # created by fetchzip.
        #
        # We resolve this by adding the missing attributes to static-nix-tools,
        # pointing back to the same static-nix-tools derivation. This allows
        # downstram derivation to keep using `nix-tools.exes.make-install-plan`
        # as shown above.
        static-nix-tools =
          let
            # TODO replace once haskell-nix-examples nix-tools is in haskell.nix
            zipFile = (import ../nix-tools-static.nix final).${final.system};
            tarball = final.runCommand "nix-tools" {
              nativeBuildInputs = [ final.unzip ];
            } ''
              mkdir -p $out/bin
              cd $out/bin
              unzip ${zipFile}
            '';
            nix-tools-provided-exes = builtins.attrNames nix-tools-pkgs.nix-tools.exes;
          in
            # add the missing exes attributes to the tarball derivation
            tarball // { exes = final.lib.genAttrs nix-tools-provided-exes (_: tarball); };

        # Version of nix-tools built with a pinned version of haskell.nix.
        pinned-nix-tools-lib = (import (final.haskell-nix.sources.flake-compat) {
            pkgs = final;
            inherit (final) system;
            src = ../nix-tools;
            override-inputs = {
              # Avoid downloading another `hackage.nix`.
              inherit (final.haskell-nix.sources) hackage;
            };
          }).defaultNix.lib;
      in
      {
        haskell-nix =
          prev.haskell-nix // {
            inherit (nix-tools-pkgs) nix-tools nix-tools-set;
            # either nix-tools from its overlay or from the tarball.
            nix-tools-unchecked = static-nix-tools;
          };
        # For use building hadrian.  This way updating anything that modifies the
        # way hadrian is built will not cause a GHC rebuild.
        pinned-haskell-nix = pinned-nix-tools-lib.haskell-nix final.system;
      });

    bootstrap = import ./bootstrap.nix;
    compiler-llvm = import ./compiler-llvm.nix;
    ghc = import ./ghc.nix;
    ghc-packages = import ./ghc-packages.nix;
    hydra = import ./hydra.nix { inherit sources; };
    darwin = import ./darwin.nix;
    windows = import ./windows.nix;
    armv6l-linux = import ./armv6l-linux.nix;
    musl = import ./musl.nix;
    android = import ./android.nix;
    tools = import ./tools.nix;
    emscripten = import ./emscripten.nix;
    nix-prefetch-git-minimal = import ./nix-prefetch-git-minimal.nix;
    gobject-introspection = import ./gobject-introspection.nix;
    hix = import ./hix.nix;
    ghcjs = import ./ghcjs.nix;
    cabalPkgConfig = import ./cabal-pkg-config.nix;
    cacheCompilerDeps = import ./cache-compiler-deps.nix;
    default-setup = import ./default-setup.nix;
    dummy-ghc-data = import ./dummy-ghc-data.nix;
    fetch-source = import ./fetch-source.nix;
  };

  composeExtensions = f: g: final: prev:
    let
      fApplied = f final prev;
      prev' = prev // fApplied;
    in fApplied // g final prev';

  ordered = with overlays; [
    # Hide nixpkgs haskell and haskellPackages from the haskell-nix overlays.
    # This should prevent us inadvertently depending on them.
    (_: prev: {
      haskell = { };
      haskellPackages = { };
      haskell-nix-prev = prev;
    })
    wine
    haskell
    nix-tools
    bootstrap
    compiler-llvm
    ghc
    ghc-packages
    darwin
    windows
    armv6l-linux
    musl
    android
    tools
    emscripten
    nix-prefetch-git-minimal
    ghcjs
    cabalPkgConfig
    gobject-introspection
    hix
    hydra
    # Restore nixpkgs haskell and haskellPackages
    (_: prev: { inherit (prev.haskell-nix-prev) haskell haskellPackages; })
    dummy-ghc-data
    cacheCompilerDeps
    default-setup
    fetch-source
  ];
  combined = builtins.foldl' composeExtensions (_: _: { }) ordered;
in overlays // { inherit combined; }

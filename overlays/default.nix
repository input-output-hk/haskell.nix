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
        static-nix-tools' = pins:
          let
            # TODO replace once haskell-nix-examples nix-tools is in haskell.nix
            zipFile = (import pins final).${final.stdenv.hostPlatform.system};
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

        static-nix-tools = static-nix-tools' ../nix-tools-static.nix;
        # Any change to default-setup requires rebuilding everthing.
        # Having a dedicated file for `default-setup` allows us to update
        # the other `nix-tools` (like `make-install-plan`), without a
        # full rebuild.
        static-nix-tools-for-default-setup = static-nix-tools' ../nix-tools-static-for-default-setup.nix;

        # Version of nix-tools built with a pinned version of haskell.nix.
        pinned-nix-tools-flake = (import final.haskell-nix.sources.flake-compat {
            pkgs = final;
            inherit (final.stdenv.hostPlatform) system;
            src = ../nix-tools;
            override-inputs = {
              # Avoid downloading another `hackage.nix`.
              inherit (final.haskell-nix.sources) hackage;
            };
          }).defaultNix;
        pinned-nix-tools-lib = pinned-nix-tools-flake.lib;

        # `static-nix-tools` is disabled for now: we build nix-tools
        # from source so that `make-install-plan` links the
        # stable-haskell cabal fork (see nix-tools/cabal.project),
        # which carries the cross-compilation "stage" system
        # (--with-build-compiler / build:/host: constraints).  The
        # pinned static tarball cannot pick that up.
        #
        # We route the from-source build through the *pinned*
        # haskell.nix (`pinned-nix-tools-lib`, same flake-compat used
        # for hadrian) rather than `nix-tools-pkgs` directly: planning
        # nix-tools with our own overlay would recurse, because
        # plan-to-nix consumes `nix-tools-unchecked` twice — the
        # overridable `nix-tools` param (make-install-plan/plan-to-nix)
        # *and* the hardcoded `cabal2json` in `dummy-ghc-pkg-dump`.
        # The pinned instance bootstraps with its own static nix-tools,
        # so the recursion is broken outside our fixpoint.
        #
        # Use the *unchecked* variant (the fork exes planned by the pinned
        # static 3.16 make-install-plan), NOT the checked one: the checked
        # variant re-plans itself with the freshly-built fork make-install-plan,
        # and the fork's stage system configures a C toolchain at plan time
        # (requireProgram gcc), which the pinned plan-to-nix env doesn't provide.
        # The unchecked variant is planned by the 3.16 static binary, which does
        # not, so the bootstrap stays clean.
        from-source-nix-tools =
          pinned-nix-tools-flake.legacyPackages.${final.stdenv.hostPlatform.system}.nix-tools-unchecked;
      in
      {
        haskell-nix =
          prev.haskell-nix // {
            inherit (nix-tools-pkgs) nix-tools nix-tools-set;
            # nix-tools built from source (with the cabal fork).
            # `default-setup` is still taken from the static pin so a
            # fork change doesn't force a global v1 rebuild.
            nix-tools-unchecked = from-source-nix-tools // {
              exes = from-source-nix-tools.exes // {
                inherit (static-nix-tools-for-default-setup.exes) default-setup default-setup-ghcjs;
              };
            };
          };
        # For use building hadrian.  This way updating anything that modifies the
        # way hadrian is built will not cause a GHC rebuild.
        pinned-haskell-nix = pinned-nix-tools-lib.haskell-nix final.stdenv.hostPlatform.system;
      });

    bootstrap = import ./bootstrap.nix;
    compiler-llvm = import ./compiler-llvm.nix;
    ghc-packages = import ./ghc-packages.nix;
    darwin = import ./darwin.nix;
    windows = import ./windows.nix;
    armv6l-linux = import ./armv6l-linux.nix;
    hyper-linux = import ./hyper-linux.nix { inherit sources; };
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
    lazy-inputs = import ../lazy-inputs;
    rcodesign = import ./rcodesign.nix;
    wasm = import ./wasm.nix;
    stable-haskell = import ./stable-haskell.nix;
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
    ghc-packages
    darwin
    windows
    armv6l-linux
    hyper-linux
    musl
    android
    tools
    emscripten
    nix-prefetch-git-minimal
    ghcjs
    cabalPkgConfig
    gobject-introspection
    hix
    wasm
    # Restore nixpkgs haskell and haskellPackages
    (_: prev: { inherit (prev.haskell-nix-prev) haskell haskellPackages; })
    lazy-inputs
    stable-haskell
    cacheCompilerDeps
    rcodesign
  ];
  combined = builtins.foldl' composeExtensions (_: _: { }) ordered;
in overlays // { inherit combined; }

# For the time being we can't really treat ghcjs like a
# regular ghc (with different target). We need this as a
# stop-gap measure until ghcjs can be treated like a regular
# ghc.
#
# `haskell-nix.ghcjsProject` wraps `haskell-nix.cabalProject'`
# for use with the ghcjs source. It is exposed to allow GHCJS
# developers to work on the GHCJS code in a nix-shell with
# `shellFor`. It takes thes the clean src (from fetched
# source or local source that has been cleaned with cleanGit.
# It configures the source and passes it onto `cabalProject`
# along with the necessary modules.
#
# It also adds `configureInputs` and `bootInputs` that a
# needed for configuring the source and booting the compiler
# once it is built.  These are added to the `hsPkgs.shellFor`
# of the project.
{ pkgs }:
{ src
, ghc ? pkgs.ghc
, ghcjsVersion ? "8.6.0.1" # TODO get this from the source?
, ghcVersion ? "8.6.5"     # TODO get this from the ghc arg?
, happy ? pkgs.haskellPackages.happy
, alex ? pkgs.haskellPackages.alex
, cabal-install ? pkgs.cabal-install
, ...
}@args:
let
    isGhcjs88 = builtins.compareVersions ghcjsVersion "8.8.0.0" > 0;

    # Inputs needed to configure the GHCJS source tree
    configureInputs = with pkgs; [
            perl
            autoconf
            automake
            python3
        ] ++ [
            ghc
            happy
            alex
            cabal-install
        ];
    # nixpkgs does not have an emsdk, this derivation uses symlinks to make something
    # that matches enought for `ghcjs-boot` to work
    emscriptenupstream = pkgs.buildPackages.emscriptenupstream;
    emscripten = pkgs.buildPackages.emscripten.override {
      emscriptenBackend = emscriptenupstream;
    };
    emsdk = pkgs.linkFarm "emsdk" [
      { name = "upstream/bin/clang"; path = emscriptenupstream + "/bin/clang"; }
      { name = "upstream/emscripten/emcc"; path = emscripten + "/bin/emcc"; }
      { name = "upstream/emscripten/emar"; path = emscripten + "/bin/emar"; }
      { name = "upstream/emscripten/emranlib"; path = emscripten + "/bin/emranlib"; }
      { name = "share"; path = emscripten + "/share"; }
    ];
    # Inputs needed to boot the GHCJS compiler
    bootInputs = with pkgs; [
            nodejs
            makeWrapper
            xorg.lndir
            gmp
            pkgconfig
        ]
        ++ [ ghc cabal-install ]
        ++ lib.optionals stdenv.isDarwin [
          pkgs.buildPackages.gcc # https://github.com/ghcjs/ghcjs/issues/663
        ]
        ++ lib.optional isGhcjs88 [ emscripten ];
    # Configured the GHCJS source
    configured-src = pkgs.runCommand "configured-ghcjs-src" {
        buildInputs = configureInputs;
        inherit src;
        } ''
        export HOME=$(pwd)
        mkdir $HOME/.cabal
        touch $HOME/.cabal/config
        cp -r "$src" "$out"
        chmod -R +w "$out"
        cd "$out"

        # TODO: Find a better way to avoid impure version numbers
        sed -i 's/RELEASE=NO/RELEASE=YES/' ghc/configure.ac
        sed -i 's/${ghcjsVersion}/${ghcVersion}/' ghcjs.cabal

        ${
          # TODO: How to actually fix this?
          # Seems to work fine and produce the right files.
          pkgs.lib.optionalString (!isGhcjs88) ''
            touch ghc/includes/ghcautoconf.h
            mkdir -p ghc/compiler/vectorise
            mkdir -p ghc/utils/haddock/haddock-library/vendor
          ''
        }

        patchShebangs .
        sed -i 's/gcc /cc /g' utils/makePackages.sh
        cat utils/makePackages.sh
        ./utils/makePackages.sh copy

        echo "    build-tool-depends: alex:alex, happy:happy <= 1.19.9" >> lib/ghc-api-ghcjs/ghc-api-ghcjs.cabal

        # nuke the HsBaseConfig.h from base.buildinfo.in; this will
        # prevent it from being installed and provide incorrect values.
        sed -i 's/HsBaseConfig.h//g' lib/boot/pkg/base/base.buildinfo.in
        cat lib/boot/pkg/base/base.buildinfo.in
        '';
        # see https://github.com/ghcjs/ghcjs/issues/751 for the happy upper bound.

    ghcjsProject = pkgs.haskell-nix.cabalProject' (
        (pkgs.lib.filterAttrs 
            (n: _: builtins.any (x: x == n)
                ["src" "ghcjsVersion" "ghcVersion" "happy" "alex" "cabal-install"]) args) // {
        src = configured-src;
        ghc = ghc.buildGHC;
        configureArgs = pkgs.lib.optionalString isGhcjs88 "--constraint='Cabal >=3.0.2.0 && <3.1'";
        modules = [
            {
                # we need ghc-boot in here for ghcjs.
                nonReinstallablePkgs = [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
                                         "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
                                         "ghc-boot" "binary" "bytestring" "filepath" "directory" "containers"
                                         "time" "unix" "Win32" ];
            }
            (pkgs.lib.optionalAttrs (!isGhcjs88) {
                packages.Cabal.patches = [ ./../overlays/patches/Cabal/fix-data-dir.patch ];
            })
            {
                packages.ghcjs.doHaddock = false;
                packages.haddock-ghcjs.doHaddock = false;
                packages.haddock-api-ghcjs.doHaddock = false;
                packages.ghcjs.flags.no-wrapper-install = true;
                # set use-host-template-haskell. This *does*
                # work as we use a patched ghc to boot anyway.
                # (we apply https://github.com/ghcjs/ghc/commit/2918d88d4ef786b5f2801f6f77ac333cc56dde75 already)
                packages.ghcjs.flags.use-host-template-haskell = true;
                packages.ghc-api-ghcjs.flags.use-host-template-haskell = true;
                packages.ghcjs-th.flags.use-host-template-haskell = true;
                packages.ghc.flags.ghci = true;
                packages.ghci.flags.ghci = true;
                # packages.ghcjs.components.library.configureFlags = [ "-fno-wrapper-install" ];
                packages.ghcjs.components.library.build-tools = [ alex ];
            }
        ];
    });
in ghcjsProject // {
    # Add `configureInputs` and `bootInputs` to the shell
    hsPkgs = ghcjsProject.hsPkgs // {
        # Shell suitable for configuring and building a local copy of GHCJS
        shellFor = args: (ghcjsProject.hsPkgs.shellFor args).overrideAttrs (drv: {
            buildInputs = (drv.buildInputs or []) ++ configureInputs;
            nativeBuildInputs = (drv.nativeBuildInputs or []) ++ bootInputs;
            EMSDK = emsdk;
        });
    };
    inherit configureInputs bootInputs configured-src emsdk;
}


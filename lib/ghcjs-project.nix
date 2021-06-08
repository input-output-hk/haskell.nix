# IMPORTANT: This is for building ghcjs, not for building with
# ghcjs. To build with ghcjs then just replace `haskell-nix`
# with `pkgsCross.ghcjs.haskell-nix`.
#
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
, compiler-nix-name
, ghc ? pkgs.buildPackages.haskell-nix.compiler.${compiler-nix-name}
, ghcjsVersion
, ghcVersion ? ghc.version
, happy ? pkgs.haskell-nix.tool compiler-nix-name "happy" {
    index-state = pkgs.haskell-nix.internalHackageIndexState;
    version = "1.19.12";
    materialized = ../materialized/ghcjs/happy + "/${compiler-nix-name}";
  }
, alex ? pkgs.haskell-nix.tool compiler-nix-name "alex" {
    index-state = pkgs.haskell-nix.internalHackageIndexState;
    version = "3.2.5";
    materialized = ../materialized/ghcjs/alex + "/${compiler-nix-name}";
  }
, cabal-install ?
  if (builtins.compareVersions ghcjsVersion "8.10.0.0" >= 0)
  then pkgs.haskell-nix.tool compiler-nix-name "cabal" {
    index-state = pkgs.haskell-nix.internalHackageIndexState;
    version = "3.4.0.0";
    materialized = ../materialized/ghcjs/cabal + "/${compiler-nix-name}";
  }
  else pkgs.haskell-nix.tool compiler-nix-name "cabal" {
    index-state = pkgs.haskell-nix.internalHackageIndexState;
    version = "3.2.0.0";
    # Cabal 3.2.1.0 no longer supports he mix of `cabal-version`,
    # lack of `custom-setup` and `v1-install` used by ghcjs boot.
    cabalProjectLocal = ''
      constraints: Cabal <3.2.1.0
    '';
    materialized = ../materialized/ghcjs/cabal + "/${compiler-nix-name}";
  }
, ...
}@args:
let
    isGhcjs88 = builtins.compareVersions ghcjsVersion "8.8.0.0" >= 0;
    isGhcjs810 = builtins.compareVersions ghcjsVersion "8.10.0.0" >= 0;

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

    inherit (pkgs.buildPackages) emscriptenupstream emscripten emsdk;

    # Inputs needed to boot the GHCJS compiler
    bootInputs = with pkgs.buildPackages; [
            nodejs
            makeWrapper
            xorg.lndir
            gmp
            pkgconfig
        ]
        ++ [ ghc cabal-install emsdk ];
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

        rm -rf utils/pkg-cache/ghc
        cp -r ${ghc.generated} utils/pkg-cache/ghc

        cp ${../overlays/patches/config.sub} ghc/libraries/integer-gmp/config.sub
        cp ${../overlays/patches/config.sub} ghc/libraries/base/config.sub
        cp ${../overlays/patches/config.sub} ghc/libraries/unix/config.sub

        sed -i 's/_AC_PROG_CC_C99/AC_PROG_CC_C99/' ghc/aclocal.m4

        patchShebangs .
        sed -i 's/gcc /cc /g' utils/makePackages.sh
        ./utils/makePackages.sh copy

        '';
        # see https://github.com/ghcjs/ghcjs/issues/751 for the happy upper bound.

    ghcjsProject = pkgs.haskell-nix.cabalProject' (
        (pkgs.lib.filterAttrs 
            (n: _: !(builtins.any (x: x == n)
                ["src" "ghcjsVersion" "ghcVersion" "happy" "alex" "cabal-install"])) args) // {
        src = configured-src;
        index-state = "2021-03-20T00:00:00Z";
        compiler-nix-name = if isGhcjs810 then "ghc8104" else if isGhcjs88 then "ghc884" else "ghc865";
        configureArgs = pkgs.lib.optionalString (isGhcjs88 && !isGhcjs810) "--constraint='Cabal >=3.0.2.0 && <3.1'";
        materialized = ../materialized + (if isGhcjs810 then "/ghcjs8104" else if isGhcjs88 then "/ghcjs884" else "/ghcjs865");
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
    inherit configureInputs bootInputs configured-src emscriptenupstream emscripten emsdk;
}


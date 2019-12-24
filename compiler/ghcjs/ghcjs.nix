{ pkgs
, ghcjsSrcJson ? ./ghcjs-src.json
, ghcjsSrc ? pkgs.buildPackages.fetchgit (builtins.fromJSON (builtins.readFile ghcjsSrcJson))
, ghcjsVersion ? "8.6.0.1"
, ghcVersion ? "8.6.5"
, ghc ? pkgs.buildPackages.ghc
, happy ? pkgs.buildPackages.haskellPackages.happy
, alex ? pkgs.buildPackages.haskellPackages.alex
, cabal-install ? pkgs.buildPackages.cabal-install
}:
let
    configured-src = pkgs.buildPackages.runCommand "configured-ghcjs-src" {
        buildInputs = with pkgs.buildPackages; [
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
        inherit ghcjsSrc;
        } ''
        export HOME=$(pwd)
        mkdir $HOME/.cabal
        touch $HOME/.cabal/config
        cp -r "$ghcjsSrc" "$out"
        chmod -R +w "$out"
        cd "$out"

        # TODO: Find a better way to avoid impure version numbers
        sed -i 's/RELEASE=NO/RELEASE=YES/' ghc/configure.ac
        sed -i 's/${ghcjsVersion}/${ghcVersion}/' ghcjs.cabal

        # TODO: How to actually fix this?
        # Seems to work fine and produce the right files.
        touch ghc/includes/ghcautoconf.h
        mkdir -p ghc/compiler/vectorise
        mkdir -p ghc/utils/haddock/haddock-library/vendor

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
    ghcjs = (pkgs.buildPackages.haskell-nix.cabalProject {
        src = configured-src;
        index-state = "2019-12-10T00:00:00Z";
        plan-sha256 = "0kkwaakkwgvs0cpxkaw3w52yxl55y8scrdj4q83w7m07dpjymqph";
        materialized = ../../materialized/ghcjs;
        inherit ghc;
        modules = [
            {
                # we need ghc-boot in here for ghcjs.
                nonReinstallablePkgs = [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
                                         "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
                                         "ghc-boot" "binary" "bytestring" "filepath" "directory" "containers"
                                         "time" "unix" "Win32" ];
            }
            {
                packages.Cabal.patches = [ ./../../overlays/patches/Cabal/fix-data-dir.patch ];
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
            }
        ];
    }).ghcjs; # <- we are only interested in the `ghcjs` package.

    all-ghcjs = pkgs.buildPackages.symlinkJoin {
        name = "ghcjs-${ghcjsVersion}-symlinked";
        paths = [
            ghcjs.components.exes.ghcjs
            ghcjs.components.exes.ghcjs-pkg
            ghcjs.components.exes.haddock-ghcjs
            ghcjs.components.exes.hsc2hs-ghcjs
            ghcjs.components.exes.ghcjs-boot
            ghcjs.components.exes.ghcjs-run
        ];
    };
    libexec = "${all-ghcjs}/libexec/${builtins.replaceStrings ["darwin" "i686"] ["osx" "i386"] pkgs.stdenv.buildPlatform.system}-${ghc.name}/ghcjs-${ghcVersion}";
in pkgs.stdenv.mkDerivation {
    name = "ghcjs-${ghcVersion}";
    src = configured-src;

    nativeBuildInputs = with pkgs.buildPackages; [
        nodejs
        makeWrapper
        xorg.lndir
        gmp
        pkgconfig
    ]
    ++ [ ghc cabal-install ]
    ++ lib.optionals stdenv.isDarwin [
      pkgs.buildPackages.gcc # https://github.com/ghcjs/ghcjs/issues/663
    ];
    passthru = {
        inherit all-ghcjs;
        inherit configured-src;
        # Used to detect non haskell-nix compilers (accedental use of nixpkgs compilers can lead to unexpected errors)
        isHaskellNixCompiler = true;
    } // ghcjs.components.exes;
    dontConfigure = true;
    dontInstall = true;
    buildPhase = ''
      export HOME=$TMP
      mkdir $HOME/.cabal
      touch $HOME/.cabal/config
      cd lib/boot

      mkdir -p $out/bin
      mkdir -p $out/lib/ghcjs-${ghcVersion}
      lndir ${libexec} $out/bin

      wrapProgram $out/bin/ghcjs --add-flags "-B$out/lib/ghcjs-${ghcVersion}"
      wrapProgram $out/bin/haddock-ghcjs --add-flags "-B$out/lib/ghcjs-${ghcVersion}"
      wrapProgram $out/bin/ghcjs-pkg --add-flags "--global-package-db=$out/lib/ghcjs-${ghcVersion}/package.conf.d"

      env PATH=$out/bin:$PATH $out/bin/ghcjs-boot -j1 --with-ghcjs-bin $out/bin
    '';
    # We hard code -j1 as a temporary workaround for
    # https://github.com/ghcjs/ghcjs/issues/654
    # enableParallelBuilding = true;
}
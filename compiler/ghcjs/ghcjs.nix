{ pkgs
, ghcjsSrcJson ? ./ghcjs-src.json
, ghcjsSrc ? pkgs.buildPackages.fetchgit (builtins.fromJSON (builtins.readFile ghcjsSrcJson))
, ghcjsVersion ? "8.6.0.1"
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
        '';
        # see https://github.com/ghcjs/ghcjs/issues/751 for the happy upper bound.
    ghcjs = (pkgs.buildPackages.haskell-nix.cabalProject {
        src = configured-src;
        inherit ghc;
        modules = [
            {
                packages.Cabal.patches = [ ./../../overlays/patches/Cabal/fix-data-dir.patch ];
                packages.ghcjs.doHaddock = false;
                packages.haddock-ghcjs.doHaddock = false;
                packages.haddock-api-ghcjs.doHaddock = false;
                packages.ghcjs.flags = { no-wrapper-install = true; };
                # packages.ghcjs.components.library.configureFlags = [ "-fno-wrapper-install" ];
            }
            {
                # we need ghc-boot in here for ghcjs.
                nonReinstallablePkgs = [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
                                         "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
                                         "ghc-boot" "binary" "bytestring" "filepath" "directory" "containers"
                                         "time" "unix" "Win32" ];
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
    libexec = "${all-ghcjs}/libexec/${builtins.replaceStrings ["darwin" "i686"] ["osx" "i386"] pkgs.stdenv.buildPlatform.system}-${ghc.name}/ghcjs-${ghcjsVersion}";
in pkgs.stdenv.mkDerivation {
    name = "ghcjs-${ghcjsVersion}";
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
    } // ghcjs.components.exes;
    dontConfigure = true;
    dontInstall = true;
    buildPhase = ''
      export HOME=$TMP
      mkdir $HOME/.cabal
      touch $HOME/.cabal/config
      cd lib/boot

      mkdir -p $out/bin
      mkdir -p $out/lib/ghcjs-${ghcjsVersion}
      lndir ${libexec} $out/bin

      wrapProgram $out/bin/ghcjs --add-flags "-B$out/lib/ghcjs-${ghcjsVersion}"
      wrapProgram $out/bin/haddock-ghcjs --add-flags "-B$out/lib/ghcjs-${ghcjsVersion}"
      wrapProgram $out/bin/ghcjs-pkg --add-flags "--global-package-db=$out/lib/ghcjs-${ghcjsVersion}/package.conf.d"

      env PATH=$out/bin:$PATH $out/bin/ghcjs-boot -j1 --with-ghcjs-bin $out/bin
    '';
    # We hard code -j1 as a temporary workaround for
    # https://github.com/ghcjs/ghcjs/issues/654
    # enableParallelBuilding = true;
}
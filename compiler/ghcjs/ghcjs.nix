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
    project = pkgs.buildPackages.haskell-nix.ghcjsProject {
        src = ghcjsSrc;
        inherit ghc ghcjsVersion ghcVersion happy alex cabal-install;
        index-state = "2019-12-10T00:00:00Z";
#        plan-sha256 = "1wy2lr08maxyi7r8jiwf2gj6pdayk5vxxwh42bj4s2gg4035z0yc";
#        materialized = ../../materialized/ghcjs;
    };

    inherit (project.hsPkgs) ghcjs;

    all-ghcjs = pkgs.buildPackages.symlinkJoin {
        name = "ghcjs-${ghcjsVersion}-symlinked";
        paths = [
            ghcjs.components.exes.ghcjs
            ghcjs.components.exes.ghcjs-pkg
            ghcjs.components.exes.haddock-ghcjs
            ghcjs.components.exes.hsc2hs-ghcjs
            ghcjs.components.exes.ghcjs-boot
            ghcjs.components.exes.ghcjs-run
            ghcjs.components.exes.ghcjs-dumparchive
        ];
    };
    libexec = "libexec/${builtins.replaceStrings ["darwin" "i686"] ["osx" "i386"] pkgs.stdenv.buildPlatform.system}-${ghc.name}/ghcjs-${ghcVersion}";
    bashScriptParentDir = ''"$(dirname "$(dirname "$(readlink -f "$0")")")"'';
in pkgs.stdenv.mkDerivation {
    name = "ghcjs-${ghcVersion}";
    src = project.configured-src;

    nativeBuildInputs = project.bootInputs;
    passthru = {
        inherit all-ghcjs;
        inherit (project) configured-src;
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
        cp ${all-ghcjs}/${libexec}/* $out/bin
        cp ${pkgs.coreutils}/bin/readlink $out/bin
        chmod -R +w $out/bin
        strip $out/bin/*
      '' + (pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
        cp ${pkgs.gmp}/lib/libgmp.10.dylib $out/bin
        cp ${pkgs.ncurses}/lib/libncursesw.6.dylib $out/bin
        cp ${pkgs.libffi}/lib/libffi.6.dylib $out/bin

        chmod -R +w $out/bin
        install_name_tool -id "@executable_path/libgmp.10.dylib" "$out/bin/libgmp.10.dylib"
        install_name_tool -id "@executable_path/libncursesw.6.dylib" "$out/bin/libncursesw.6.dylib"
        install_name_tool -id "@executable_path/libffi.6.dylib" "$out/bin/libffi.6.dylib"
        for fn in $out/bin/*; do
          install_name_tool -change "${pkgs.libiconv}/lib/libiconv.dylib" /usr/lib/libiconv.dylib "$fn"
          install_name_tool -change "${pkgs.stdenv.libc}/lib/libSystem.B.dylib" /usr/lib/libSystem.B.dylib "$fn"
          install_name_tool -change "${pkgs.gmp}/lib/libgmp.10.dylib" "@executable_path/libgmp.10.dylib" "$fn"
          install_name_tool -change "${pkgs.ncurses}/lib/libncursesw.6.dylib" "@executable_path/libncursesw.6.dylib" "$fn"
          install_name_tool -change "${pkgs.libffi}/lib/libffi.6.dylib" "@executable_path/libffi.6.dylib" "$fn"
        done
      '') + ''
        wrapProgram $out/bin/ghcjs --add-flags '"-B${bashScriptParentDir}/lib/ghcjs-${ghcVersion}"'
        wrapProgram $out/bin/haddock-ghcjs --add-flags '"-B${bashScriptParentDir}/lib/ghcjs-${ghcVersion}"'
        wrapProgram $out/bin/ghcjs-pkg --add-flags '"--global-package-db=${bashScriptParentDir}/lib/ghcjs-${ghcVersion}/package.conf.d"'

        env PATH=$out/bin:$PATH $out/bin/ghcjs-boot -j1 --with-ghcjs-bin $out/bin
    '';
    # We hard code -j1 as a temporary workaround for
    # https://github.com/ghcjs/ghcjs/issues/654
    # enableParallelBuilding = true;
}

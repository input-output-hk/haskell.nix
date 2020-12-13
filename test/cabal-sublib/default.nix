# Test a package set
{ stdenv, util, cabalProject', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name }:

with stdenv.lib;

let
  modules = [
    {
      # Package has no exposed modules which causes
      #   haddock: No input file(s)
      packages.cabal-sublib.doHaddock = false;
    }
    # TODO fix plan-to-nix so this is not needed.
    # This is a manual work around for `plan-to-nix` not
    # handling `build-depends: cabal-sublib:slib` correctly
    ({config, ...}: {
      packages.cabal-sublib.components.exes.cabal-sublib.depends = [
        config.hsPkgs.cabal-sublib.components.sublibs.slib ];
    })
    (optionalAttrs (compiler-nix-name == "ghc865") ({config, ...}: {
      reinstallableLibGhc = true;
      packages.cabal-sublib.package.buildType = mkForce "Custom";
      packages.cabal-sublib.package.setup-depends = [config.hsPkgs.Cabal];
    }))
  ];

  # The ./pkgs.nix works for linux & darwin, but not for windows
  project = cabalProject' {
    inherit compiler-nix-name;
    src = testSrc "cabal-sublib";
    inherit modules;
    pkg-def-extras = optional (compiler-nix-name == "ghc865")
      (hackage: {
        packages = {
          "Cabal" = (((hackage.Cabal)."3.2.1.0").revisions).default;
        };
      });
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "cabal-sublib-test";

    buildCommand = ''
      exe="${packages.cabal-sublib.components.exes.cabal-sublib.exePath}"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.cabal-sublib.components.exes.cabal-sublib}/test-stdout

    '' +
    # Musl and Aarch are statically linked..
    optionalString (!stdenv.hostPlatform.isAarch32 && !stdenv.hostPlatform.isAarch64 && !stdenv.hostPlatform.isMusl) (''
      printf "checking that executable is dynamically linked to system libraries... " >& 2
    '' + optionalString (stdenv.isLinux && !stdenv.hostPlatform.isMusl) ''
      ldd $exe | grep libgmp
    '' + optionalString stdenv.isDarwin ''
      otool -L $exe |grep .dylib
    '') + ''

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit packages;
    };
  };
}

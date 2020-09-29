{ stdenv, cabalProject', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name }:

with stdenv.lib;

let

  hackage = import ./hackage;

  tarball = {
    name = "extra-hackage-demo";
    index = ./01-index.tar.gz;
  };

  demo-src = ./external-package-demo-0.1.0.0.tar.gz;

  project = cabalProject' {
    inherit compiler-nix-name;
    src = testSrc "extra-hackage/external-package-user";

    extra-hackages = [ hackage ];
    extra-hackage-tarballs = [ tarball ];

    modules = [
      # To prevent nix-build from trying to download it from the
      # actual Hackage.
      { packages.external-package-demo.src = demo-src; }
    ];
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "external-hackage-test";

    buildCommand = ''
      exe="${packages.external-package-user.components.exes.external-package-user}/bin/external-package-user${stdenv.hostPlatform.extensions.executable}"
      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2
      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.external-package-user.components.exes.external-package-user}/test-stdout
    '' + (if stdenv.hostPlatform.isMusl
      then ''
        printf "checking that executable is statically linked... " >& 2
        (ldd $exe 2>&1 || true) | grep -i "not a"
      ''
      else
        # Skip this on aarch as we do not have an `ldd` tool
        optionalString (!stdenv.hostPlatform.isAarch32 && !stdenv.hostPlatform.isAarch64) (''
          printf "checking that executable is dynamically linked to system libraries... " >& 2
        '' + optionalString stdenv.isLinux ''
          ldd $exe | grep libpthread
        '' + optionalString stdenv.isDarwin ''
          otool -L $exe |grep .dylib
      '')) + ''
      touch $out
    '';
    meta.platforms = platforms.all;
    passthru = {
      inherit project;
    };
  };
}

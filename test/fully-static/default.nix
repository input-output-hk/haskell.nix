{ stackProject'
, stdenv, gmp6, openssl, zlib, libffi
, buildPackages
, recurseIntoAttrs
}:

with stdenv.lib;

let
  # Grab the compiler name from stack-to-nix output.
  # compiler = (stack-pkgs.extras {}).compiler.nix-name;
  compiler = "ghc865";  # fixme

  # IFD stack-to-nix
  project = { gpl ? true }: stackProject' {
    src = ./.;
    pkg-def-extras = [];
    modules = [
      # Musl libc fully static build
      (let
        staticLibs = [
          zlib.static
          (openssl.override { static = true; }).out
          (libffi.overrideAttrs (oldAttrs: {
            dontDisableStatic = true;
            configureFlags = (oldAttrs.configureFlags or []) ++ [
              "--enable-static"
              "--disable-shared"
            ];
          }))
        ] ++ optional gpl (gmp6.override { withStatic = true; });

        withFullyStatic = {
          configureFlags =
             optionals stdenv.hostPlatform.isMusl ([
               "--disable-executable-dynamic"
               "--disable-shared"
               "--ghc-option=-optl=-pthread"
               "--ghc-option=-optl=-static"
             ] ++ map (drv: "--ghc-option=-optl=-L${drv}/lib") staticLibs);
        };
      in {
        # Select a non-GMP compiler, usually for software licensing reasons.
        ghc.package = mkIf (stdenv.hostPlatform.isMusl && !gpl)
            buildPackages.haskell-nix.compiler.integer-simple.${compiler};

        # Add GHC flags and libraries for fully static build
        packages.pandoc.components.exes.pandoc = withFullyStatic;
      })
    ];
  };
  packagesGmp = (project { gpl = true; }).hsPkgs;
  packagesIntegerSimple = (project { gpl = false; }).hsPkgs;

in recurseIntoAttrs {
  stack-nix-gmp = (project { gpl = true; }).stack-nix;
  stack-nix-simple = (project { gpl = false; }).stack-nix;
  run = stdenv.mkDerivation {
    name = "fully-static-test";

    depsBuildBuild = [ buildPackages.file ];

    buildCommand = flip concatMapStrings
      [ packagesGmp /* packagesIntegerSimple */ ]
      (packages: ''
        exe="${packages.pandoc.components.exes.pandoc}/bin/pandoc${stdenv.hostPlatform.extensions.executable}"

        printf "checking whether executable runs... " >& 2
        ${toString packages.pandoc.components.exes.pandoc.config.testWrapper} $exe --version

      '' + optionalString stdenv.hostPlatform.isMusl ''
        printf "checking whether executable is static... " >& 2
        file $exe
        file $exe | grep "statically linked"

      '') + "touch $out";

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit buildPackages;
      project-gmp = project { gpl = true; };
      project-integer-simple = project { gpl = false; };
      pandoc-gmp = packagesGmp.pandoc.components.exes.pandoc;
      pandoc-integer-simple = packagesIntegerSimple.pandoc.components.exes.pandoc;
    };
  };
}
{ mkStackPkgSet, callStackToNix, importAndFilterProject
, stdenv, gmp6, openssl, zlib, libffi
, buildPackages
}:

with stdenv.lib;

let
  # IFD stack-to-nix
  stack-pkgs = (importAndFilterProject (callStackToNix {
    src = ./.;
  })).pkgs;

  # Grab the compiler name from stack-to-nix output.
  # compiler = (stack-pkgs.extras {}).compiler.nix-name;
  compiler = "ghc865";  # fixme

  pkgSet = { gpl ? true }: mkStackPkgSet {
    inherit stack-pkgs;
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
            buildPackages.haskell.compiler.integer-simple.${compiler};

        # Add GHC flags and libraries for fully static build
        packages.pandoc.components.exes.pandoc = withFullyStatic;
      })
    ];
  };
  packagesGmp = (pkgSet { gpl = true; }).config.hsPkgs;
  packagesIntegerSimple = (pkgSet { gpl = false; }).config.hsPkgs;
in
  stdenv.mkDerivation {
    name = "fully-static-test";

    depsBuildBuild = [ buildPackages.file ];

    buildCommand = flip concatMapStrings
      [ packagesGmp /* packagesIntegerSimple */ ]
      (packages: ''
        exe="${packages.pandoc.components.exes.pandoc}/bin/pandoc"

        printf "checking whether executable runs... " >& 2
        $exe --version

      '' + optionalString stdenv.hostPlatform.isMusl ''
        printf "checking whether executable is static... " >& 2
        file $exe
        file $exe | grep "statically linked"

      '') + "touch $out";

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet buildPackages stack-pkgs;
      pandoc-gmp = packagesGmp.pandoc.components.exes.pandoc;
      pandoc-integer-simple = packagesIntegerSimple.pandoc.components.exes.pandoc;
    };
  }

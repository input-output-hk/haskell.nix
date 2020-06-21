{ src, stdenv, pkgs, version, ghc, alex, happy, hscolour }:
stdenv.mkDerivation {

    inherit version;

    name = "ghc-source-dist";

    buildInputs = [ ghc alex happy hscolour ] ++ (with pkgs; [ automake bash git cacert python3 autoconf xorg.lndir ]);

    inherit src;

    phases = [ "unpackPhase" "buildPhase" "installPhase" ];

    buildPhase = ''
    # Happy and Alex are confused about their data dirs. See
    # https://github.com/haskell/cabal/issues/5862 for details.
    #
    # This is fixed in more recent builds.  We can't rely on
    # custom built tools as computing the source-dist is pretty
    # much in the bootstrap lane.

    export alex_datadir=$(find ${alex}/share -name "alex-*" -type d -and -not -path "*doc*")
    export happy_datadir=$(find ${happy}/share -name "happy-*" -type d -and -not -path "*doc*")

    # We need to --skip-url-rewrites, ghc is a bit too strict here.
    ./boot --skip-url-rewrites
    # let's not flood the screen with garbage.
    ./configure --silent

    # this folder ends up being needed for the windows dists; just mock it.
    mkdir ghc-tarballs
    make sdist --silent
    '';

    installPhase = ''
    mkdir -p $out
    install -Dm644 sdistprep/ghc-$(cat VERSION)-src.tar.xz       $out/src.tar.xz
    install -Dm644 sdistprep/ghc-$(cat VERSION)-testsuite.tar.xz $out/testsuite.tar.xz
    '';
}
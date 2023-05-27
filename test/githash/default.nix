{ stdenv, lib, haskell-nix, haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, runCommand, gitMinimal, buildPackages }:

with lib;

let
  src = testSrc "githash";
  git =
    # Using the cross compiled version here, but currently git does not
    # seem to cross compile (so this test is disabled for cross compilation in
    # the test/default.nix file).
    # Using buildPackages here is not right, but at least gets musl64 test to pass.
    if stdenv.hostPlatform != stdenv.buildPlatform
      then buildPackages.buildPackages.gitReallyMinimal
      else gitMinimal;
  project = haskell-nix.cabalProject' {
    inherit src;
    # When haskell.nix has come from the store (e.g. on hydra) we need to provide
    # a suitable mock of the cleaned source with a .git dir.
    modules = (optional (!(src ? origSrc && __pathExists (src.origSrc + "/.git"))) {
      packages.githash-test.src =
        rec {
          origSrc = evalPackages.runCommand "githash-test-src" { nativeBuildInputs = [ evalPackages.gitReallyMinimal ]; } ''
            mkdir -p $out/test/githash
            cd $out
            git init
            cp -r ${src}/* test/githash
            git add test/githash
            git -c "user.name=unknown" -c "user.email=unknown" commit -m 'Initial Commit'
          '';
          origSubDir = "/test/githash";
          origSrcSubDir = origSrc + origSubDir;
          outPath = origSrcSubDir;
        };
      }) ++ [{
        packages.githash-test.components.exes.githash-test.build-tools = mkForce [ git ];
      }];
    inherit compiler-nix-name evalPackages;
  };

  packages = project.hsPkgs;
  githash-test =
    packages.githash-test.components.exes.githash-test;

in recurseIntoAttrs {
  # githash runs git from TH code and this needs a cross compiled git exe
  # to work correctly.  Cross compiling git is currently brocken.
  meta.disabled = __elem compiler-nix-name ["ghc901" "ghc902"] || haskellLib.isCrossHost ||
    # TODO find out why TH fails for this
    (__elem compiler-nix-name ["ghc927" "ghc928"] && stdenv.hostPlatform.isAarch64 && stdenv.hostPlatform.isMusl);

  ifdInputs = {
    inherit (project) plan-nix;
  };

  run = stdenv.mkDerivation {
    name = "run-githash-test";

    buildCommand = ''
      exe="${githash-test}/bin/githash-test${stdenv.hostPlatform.extensions.executable}"
      echo Checking that the error message is generated and that it came from the right place:
      (${toString githash-test.config.testWrapper} $exe || true) 2>&1 \
        | grep "error, called at src/Main.hs:5:13 in main:Main"
      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
}

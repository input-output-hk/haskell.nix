{ stdenv, lib, haskell-nix, haskellLib, testSrc, compiler-nix-name, evalPackages, runCommand, gitReallyMinimal, buildPackages }:

with lib;

let
  src = testSrc "githash";
  git =
    # Using the cross compiled version here, but currently git does not
    # seem to cross compile (so this test is disabled for cross compilation in
    # the test/default.nix file).
    # Using buildPackages here is not right, but at least gets musl64 test to pass.
    if stdenv.hostPlatform != stdenv.buildPlatform && !stdenv.hostPlatform.isMusl
      then buildPackages.buildPackages.gitReallyMinimal
      else gitReallyMinimal;
  # Mock a project root that contains a `.git` directory at the top
  # and the package source at `test/githash/`.  `tGitInfoCwd` from
  # `githash` walks up from CWD at TH-eval time looking for `.git`.
  projectRoot = evalPackages.runCommand "githash-test-src" {
    nativeBuildInputs = [ evalPackages.gitReallyMinimal ];
  } ''
    mkdir -p $out/test/githash
    cd $out
    git init
    cp -r ${src}/* test/githash
    git add test/githash
    git -c "user.name=unknown" -c "user.email=unknown" commit -m 'Initial Commit'
  '';
  # The v1 comp-builder cp'd the whole projectRoot (`.git` included)
  # into the build env and `cd`d to the package subdir, so `tGitInfoCwd`
  # walking up from `test/githash/` found the parent `.git`.  v2's
  # slice tarballs only the package subdir, so the parent `.git` is
  # lost.  Sidestep the divergence by synthesising a package-root drv
  # that copies just `test/githash/` to its root AND a minimal `.git`
  # at its root containing only what githash actually reads at
  # TH-eval: HEAD (current ref), refs/ (ref → commit sha), and
  # objects/ (the commit + tree + blobs).  Index/config/hooks/logs
  # are not needed by the `rev-parse` / `log` / `rev-list` queries
  # githash issues.
  packageRoot = evalPackages.runCommand "githash-package-root" { } ''
    mkdir -p $out/.git
    cp -rL ${projectRoot}/test/githash/. $out/
    cp -r ${projectRoot}/.git/HEAD    $out/.git/HEAD
    cp -r ${projectRoot}/.git/refs    $out/.git/refs
    cp -r ${projectRoot}/.git/objects $out/.git/objects
  '';
  project = haskell-nix.cabalProject' {
    # Disable the default haskell.nix source-cleaning pass so the
    # `.git` directory inside `packageRoot` survives into the build
    # env.  See `lib/clean-source-with.nix` — passing an attrset
    # with `outPath` + `filterPath` tells haskell.nix the source is
    # already filtered and to skip its own cleaner.
    src = { outPath = packageRoot; filterPath = { path, ... }: path; };
    cabalProjectLocal = builtins.readFile ../cabal.project.local;
    modules = [{
      packages.githash-test.components.exes.githash-test.build-tools = mkForce [ git ];
    }];
    inherit compiler-nix-name evalPackages;
  };

  packages = project.hsPkgs;
  githash-test =
    packages.githash-test.components.exes.githash-test;

in lib.recurseIntoAttrs {
  # githash runs git from TH code and this needs a cross compiled git exe
  # to work correctly.  Cross compiling git is currently broken.
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
        | grep "error, called at src/Main.hs:5:13 in .*:Main"
      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
}

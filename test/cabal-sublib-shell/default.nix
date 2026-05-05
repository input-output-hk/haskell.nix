{ stdenv, lib, pkgs, haskellLib, haskell-nix, buildPackages, runCommand
, cabalProject', testSrc, compiler-nix-name, evalPackages }:

with lib;

# The v1 (Setup.hs-based) builder used to synthesize ghc-pkg confs
# for public sublibraries that were missing `package-name:` and
# `lib-name:`.  With that missing, an unpatched cabal-install's
# solver treats the installed sublib unit as unusable and rebuilds
# the sublib from source inside the project.  This rebuild defeats
# the whole point of using the shell's pre-built deps.
#
# Structure:
#   - provider: has a main library and a `visibility: public` sublib `slib`.
#   - consumer: an executable that depends on `provider` and `provider:slib`.
#
# Test: spin up a v1 shell, drop into it, run `cabal v2-build consumer`
# using the UNPATCHED cabal-install from nix-tools, and inspect the
# build log.  If cabal built `provider` or the `slib` sublib from
# source, the sibling bug is present.
let
  isTargetCompiler = compiler-nix-name == "ghc9141";

  project = cabalProject' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "cabal-sublib-shell";
    cabalProjectLocal = builtins.readFile ../cabal.project.local
      + lib.optionalString (haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64) ''
        constraints: text -simdutf, text source
      '';
    shell.tools.cabal = {};
    modules = [
      { packages.provider.doHaddock = false; }
    ];
    builderVersion = 2;
  };

  env = project.shellFor {
    packages = ps: [ ps.consumer ];
    withHoogle = false;
  };

in lib.recurseIntoAttrs {
  ifdInputs = { inherit (project) plan-nix; };
  inherit env;

  run = stdenv.mkDerivation {
    name = "cabal-sublib-shell-test";
    passthru = { inherit project; };

    nativeBuildInputs = env.nativeBuildInputs;

    buildCommand = ''
      export HOME=$PWD/home
      mkdir -p "$HOME/.cabal"

      # Replay the shellHook: seed ~/.cabal/store from the v2
      # shell's composed store.  Nix derivations don't source
      # shellHook themselves, so invoke the same command a user
      # would get on PATH when they enter the shell.
      haskell-nix-cabal-store-sync

      # Stage consumer, point cabal at a local repo containing
      # provider's tarball so the solver can see provider as a
      # hackage candidate in addition to the installed unit.
      cp -r ${testSrc "cabal-sublib-shell"}/consumer ./consumer
      chmod -R +w consumer
      repoDir=$PWD/repo
      mkdir -p "$repoDir"
      cp ${project.hsPkgs.provider.components.library.passthru.pkgTarball} \
         "$repoDir/provider-0.1.0.0.tar.gz"

      cd consumer
      cat > cabal.project <<EOF
      packages: .
      with-compiler: ghc-${project.pkg-set.config.compiler.version}
      active-repositories: local
      EOF
      cat > "$HOME/.cabal/config" <<EOF
      repository local
        url: file+noindex://$repoDir
      EOF

      echo "=== cabal v2-build consumer (unpatched cabal) ==="
      ${project.pkg-set.config.ghc.package.targetPrefix}cabal --config-file=$HOME/.cabal/config v2-build -v2 consumer 2>&1 | tee build.log

      echo "=== verifying solver reused provider ==="
      if grep -q 'Building library for provider-' build.log; then
        echo "FAIL: cabal rebuilt provider's library instead of reusing the shell's" >&2
        exit 1
      fi
      if grep -q "Building library 'slib' for provider-" build.log; then
        echo "FAIL: cabal rebuilt provider's 'slib' sublibrary instead of reusing the shell's" >&2
        exit 1
      fi

      touch $out
    '';

    meta = { platforms = platforms.unix; };
  };
}

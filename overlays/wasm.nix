final: prev: prev.lib.optionalAttrs prev.stdenv.targetPlatform.isWasm {
  # Remove lit's max-time.py self-test. It hangs in the nix build sandbox
  # due to a Python multiprocessing.Pool deadlock (`_help_stuff_finish`
  # acquires `inqueue._rlock` without releasing it) combined with a leaked
  # signal mask that leaves SIGTERM blocked in the worker, so `pool.terminate()`
  # can't kill it. nix-shell builds complete fine; only nix-build hangs.
  llvmPackages_21 =
    let
      f = llvmFinal: llvmPrev: {
        libllvm = llvmPrev.libllvm.overrideAttrs (old: {
          postPatch = (old.postPatch or "") + ''
            rm utils/lit/tests/max-time.py
          '';
        });
      };
      # Preserve `.override` after `overrideScope`; see nixpkgs#447012.
      override = args: (prev.llvmPackages_21.override args).overrideScope f;
    in prev.lib.makeOverridable
      (prev.lib.mirrorFunctionArgs prev.llvmPackages_21.override override)
      { };
  llvmPackages = final.llvmPackages_21.override {
    patchesFn = p: p // { "llvm/gnu-install-dirs.patch" = [{path = ./patches/wasm;}]; };
    monorepoSrc =
      final.stdenv.mkDerivation {
        pname = "llvm-source";
        version = final.llvmPackages_21.llvm.version + "-haskell";
        src = final.llvmPackages_21.llvm.monorepoSrc;
        patches = ./patches/wasm/llvm/haskell-wasm-llvm-project.patch;
        buildPhase = "true";
        installPhase = ''
          cp -r . $out
        '';
      };
  };
  wasilibc = prev.wasilibc.overrideAttrs (old: {
    version = "25";
    src = final.buildPackages.fetchFromGitLab {
      domain = "gitlab.haskell.org";
      owner = "haskell-wasm";
      repo = "wasi-libc";
      rev = "951e93b336cb0ffac3ab2874640b70ed854fdf27";
      hash = "sha256-8thdYhX4bZuU/CbBNdcab3tUnugaEvI6RDw7im4eeec=";
      fetchSubmodules = true;
    };
    preBuild = ''
      patchShebangs ./scripts
      makeFlagsArray+=(
        "default"
        "libc_so"
        "CHECK_SYMBOLS=yes"
        )
      export BUILTINS_LIB=$($CC --print-libgcc-file-name)
    '';
    postBuild = ''
      mkdir -p ${builtins.placeholder "out"}
      mkdir -p ${builtins.placeholder "dev"}
      mkdir -p ${builtins.placeholder "share"}
      cp -r sysroot/lib/wasm32-wasi ${builtins.placeholder "out"}/lib
      cp -r sysroot/include/wasm32-wasi ${builtins.placeholder "dev"}/include
      cp -r sysroot/share/wasm32-wasi ${builtins.placeholder "share"}/share
    '';
    nativeBuildInputs = old.nativeBuildInputs or [] ++ [ final.buildPackages.lld ];
  });

  haskell-nix = prev.haskell-nix // ({
    defaultModules = prev.haskell-nix.defaultModules ++ [
      ({ pkgs, ... }: {
        testWrapper = ["HOME=$(mktemp -d)" (pkgs.pkgsBuildBuild.wasmtime + "/bin/wasmtime")];
        package-keys = ["clock"];
        packages.clock.ghcOptions = ["-optc-Wno-int-conversion"];
      })
    ];
  });
}

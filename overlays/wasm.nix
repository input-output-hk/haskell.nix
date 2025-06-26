final: prev: prev.lib.optionalAttrs prev.stdenv.targetPlatform.isWasm {
  llvmPackages = final.llvmPackages_20.override {
    patchesFn = p: p // { "llvm/gnu-install-dirs.patch" = [{path = ./patches/wasm;}]; };
    monorepoSrc =
      final.stdenv.mkDerivation {
        pname = "llvm-source";
        version = final.llvmPackages_20.llvm.version + "-haskell";
        src = final.llvmPackages_20.llvm.monorepoSrc;
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
      rev = "f8f0d3101e02aa3aaf37c5e31db23de34963053d";
      hash = "sha256-EvqbvVP9EH63C7KUmN4QaYjYbc4yGPU7vNev9u6a46o=";
      fetchSubmodules = true;
    };
    preBuild = ''
      patchShebangs ./scripts
      makeFlagsArray+=(
        "default"
        "libc_so"
        ) 
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

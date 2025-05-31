final: prev: prev.lib.optionalAttrs prev.stdenv.targetPlatform.isWasm {
  llvmPackages = final.llvmPackages_20.override {
    version = "20.1.0-haskel-wasm";
    gitRelease.rev-version = "20.1.0-haskell-wasm";
    officialRelease = null;
    patchesFn = p: p // { "llvm/gnu-install-dirs.patch" = [{path = ./patches/wasm;}]; };
    monorepoSrc = final.buildPackages.fetchFromGitLab {
      domain = "gitlab.haskell.org";
      owner = "haskell-wasm";
      repo = "llvm-project";
      rev = "3af5c33f0010c300d23adff0c576c637ba381580";
      hash = "sha256-GnOP0tpyk+cWjMCJtxGi7FT78Wckl8fb6eibfdFWAJk=";
      fetchSubmodules = true;
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
}

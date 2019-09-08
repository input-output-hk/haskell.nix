[
    (import ./release-19.03.nix)
    # This overlay does *not* work as expected.
    # See https://github.com/NixOS/nixpkgs/issues/65589
    (self: super: super.lib.recursiveUpdate super {
        lib.systems.examples = {
            # Ghcjs
            ghcjs = {
                config = "js-unknown-ghcjs";
                platform = {};
            };
            # Asterius
            asterius32 = {
                config = "wasm32-unknown-asterius";
                platform = {};
            };
            asterius64 = {
                config = "wasm64-unknown-asterius";
                platform = {};
            };
        };
        # gcc = if self.targetPlatform.isGhcjs then null else super.gcc;
        lib.systems.parse = with self.lib.systems.parse; {
            cpuTypes.js = cpuTypes.wasm32 // { name = "js"; family = "js"; };
            kernels.ghcjs = kernels.none // { name = "ghcjs"; };
            kernels.asterius = kernels.none // { name = "asterius"; };
            mkSkeletonFromList = l: builtins.trace l (super.lib.systems.parse.mkSkeletonFromList l);
            mkSystemFromString = s: builtins.trace s (super.lib.systems.parse.mkSystemFromString s);
        };
        lib.systems.inspect.patterns = with self.lib.systems.parse; {
            isJavaScript = { cpu = cpuTypes.js; };
            isWasm32     = { cpu = cpuTypes.wasm32; };
            isWasm64     = { cpu = cpuTypes.wasm64; };
            isGhcjs      = { kernel = kernels.ghcjs; };
            isAsterius   = { kernel = kernels.asterius; };
        };
    })
    #(import ./python.nix)
    (import ./haskell.nix)
    (import ./bootstrap.nix)
    (import ./ghc.nix)
    (import ./ghc-packages.nix)
    (import ./windows.nix)
]
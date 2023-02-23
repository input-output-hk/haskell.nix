# This overlay does *not* work as expected.
# See https://github.com/NixOS/nixpkgs/issues/65589
final: prev: prev.lib.recursiveUpdate prev {
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
    # gcc = if final.stdenv.targetPlatform.isGhcjs then null else prev.gcc;
    lib.systems.parse = with final.lib.systems.parse; {
        cpuTypes.js = cpuTypes.wasm32 // { name = "js"; family = "js"; };
        kernels.ghcjs = kernels.none // { name = "ghcjs"; };
        kernels.asterius = kernels.none // { name = "asterius"; };
        mkSkeletonFromList = l: builtins.trace l (prev.lib.systems.parse.mkSkeletonFromList l);
        mkSystemFromString = s: builtins.trace s (prev.lib.systems.parse.mkSystemFromString s);
    };
    lib.systems.inspect.patterns = with final.lib.systems.parse; {
        isJavaScript = { cpu = cpuTypes.js; };
        isWasm32     = { cpu = cpuTypes.wasm32; };
        isWasm64     = { cpu = cpuTypes.wasm64; };
        isGhcjs      = { kernel = kernels.ghcjs; };
        isAsterius   = { kernel = kernels.asterius; };
    };
}

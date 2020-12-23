final: prev: {
  # GHCJS currently requires an old version of emscripten. Overriding to build the older version doesn't
  # work straightforwardly on 20.09 anymore, so we use an ugly hack and pull emscripten from 20.03.
  # In future, we'll just fix GHCJS to work with the newer emscripten.
  inherit (import final.haskell-nix.sources.nixpkgs-2003 {
    system = final.stdenv.system;
    overlays = [(final: prev: with final; {
      binaryen = callPackage ./emscripten/binaryen.nix {};

      emscriptenVersion = "1.39.1";

      emscripten = callPackage ./emscripten { };

      emscriptenfastcompPackages = dontRecurseIntoAttrs (callPackage ./emscripten/fastcomp { });

      emscriptenfastcomp = emscriptenfastcompPackages.emscriptenfastcomp;

      emscriptenupstreamPackages = dontRecurseIntoAttrs (callPackage ./emscripten/upstream { });

      emscriptenupstream = emscriptenupstreamPackages.emscriptenupstream;

      # emscriptenPackages = recurseIntoAttrs (callPackage ./emscripten-packages.nix { });

      emscriptenStdenv = stdenv // { mkDerivation = buildEmscriptenPackage; };
    })];
  }) emscripten emscriptenupstream;
}

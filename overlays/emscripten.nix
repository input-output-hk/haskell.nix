self: super: with self; {

  binaryen = callPackage ./emscripten/binaryen.nix {};

  emscriptenVersion = "1.39.1";

  emscripten = callPackage ./emscripten { };

  emscriptenfastcompPackages = dontRecurseIntoAttrs (callPackage ./emscripten/fastcomp { });

  emscriptenfastcomp = emscriptenfastcompPackages.emscriptenfastcomp;

  emscriptenupstreamPackages = dontRecurseIntoAttrs (callPackage ./emscripten/upstream { });

  emscriptenupstream = emscriptenupstreamPackages.emscriptenupstream;

  # emscriptenPackages = recurseIntoAttrs (callPackage ./emscripten-packages.nix { });

  emscriptenStdenv = stdenv // { mkDerivation = buildEmscriptenPackage; };
}

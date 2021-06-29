final: prev: let
  # GHCJS currently requires an old version of emscripten. Overriding to build the older version doesn't
  # work straightforwardly on 20.09 anymore, so we use an ugly hack and pull emscripten from 20.03.
  # In future, we'll just fix GHCJS to work with the newer emscripten.

  # Not sure what to do here - akrmn
  pkgs-2003 = import final.haskell-nix.sources.nixpkgs-2003 {
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
  };

in {
    # nixpkgs does not have an emsdk, this derivation uses symlinks to make something
    # that matches enought for `ghcjs-boot` to work
    emscriptenupstream = pkgs-2003.emscriptenupstream;
    emscripten = (pkgs-2003.emscripten.override {
      emscriptenBackend = pkgs-2003.emscriptenupstream;
    }).overrideAttrs (attrs: {
      buildInputs = attrs.buildInputs or [] ++ [ final.makeWrapper ];
      buildCommand = attrs.buildCommand or "" + ''
        for f in $(find $out/bin/ -type l -executable ! -name "*.py"); do
          if [[ -e "$f.py" ]]; then
            rm "$f"
            echo '#!'${final.runtimeShell} > "$f"
            echo 'if [[ ! -d $HOME ]]; then' >> "$f"
            echo '  export HOME=$(mktemp -d)' >> "$f"
            echo 'fi' >> "$f"
            echo 'exec "'$f.py'" "$@"' >> "$f"
            chmod +x "$f"
          fi
        done
      '';
    });
    emsdk = final.linkFarm "emsdk" [
      { name = "upstream/bin"; path = final.emscriptenupstream + "/bin"; }
      { name = "upstream/emscripten"; path = final.emscripten + "/bin"; }
      { name = "share"; path = final.emscripten + "/share"; }
    ];
}

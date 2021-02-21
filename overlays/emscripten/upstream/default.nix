{ newScope, stdenv, lib, binutils, wrapCCWith, symlinkJoin }:
let
  callPackage = newScope (self // {inherit stdenv;});

  self = {
    emscriptenupstream-unwrapped = callPackage ./emscripten-upstream.nix {};
    emscriptenupstream-wrapped = wrapCCWith {
      cc = self.emscriptenupstream-unwrapped;
      # Never want Apple's cctools for WASM target
      bintools = binutils;
      libc = stdenv.cc.libc;
      extraBuildCommands = ''
        # hardening flags break WASM support
        cat > $out/nix-support/add-hardening.sh
      '';
    };
    emscriptenupstream = symlinkJoin {
      name = "emscriptenupstream-${lib.getVersion self.emscriptenupstream-unwrapped}";
      paths = [ self.emscriptenupstream-wrapped self.emscriptenupstream-unwrapped ];
      preferLocalBuild = false;
      allowSubstitutes = true;
      postBuild = ''
        # replace unwrapped clang-3.9 binary by wrapper
        ln -sf $out/bin/clang $out/bin/clang-[0-9]*
      '';
    };
  };
in self

self: super:
let
  emptyDotCabal = self.runCommand "empty-dot-cabal" {
      nativeBuildInputs = [ self.cabal-install ];
    } ''
      mkdir -p $out/.cabal
      cat <<EOF > $out/.cabal/config
      EOF
    '';
  callCabalSdist = src: self.runCommand "sdist.tar.gz" {
      nativeBuildInputs = [ self.cabal-install ];
    } ''
      tmp=$(mktemp -d)
      cp -r ${src}/* $tmp
      cd $tmp
      tmp2=$(mktemp -d)
      HOME=${emptyDotCabal} cabal new-sdist -o $tmp2
      cp $tmp2/*.tar.gz $out
    '';
  callCabal2Nix = src: self.stdenv.mkDerivation {
    name = "package-nix";
    inherit src;
    nativeBuildInputs = [ self.haskell-nix.nix-tools ];
    phases = [ "unpackPhase" "buildPhase" ];
    buildPhase = ''
      cabal-to-nix *.cabal > $out
    '';
  };
  importCabal = src: import (callCabal2Nix (
    callCabalSdist src));
in {
  ghc-boot-packages = builtins.mapAttrs (name: value:
    builtins.mapAttrs (pkgName: dir: importCabal "${value.passthru.configured-src}/${dir}") {
      ghc      = "compiler";
      ghci     = "libraries/ghci";
      ghc-boot = "libraries/ghc-boot";
      libiserv = "libraries/libiserv";
      iserv    = "utils/iserv";
      # TODO iserv-proxy ?
    }
  ) self.haskell.compiler;
}

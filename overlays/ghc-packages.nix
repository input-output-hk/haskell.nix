self: super:
let
  emptyDotCabal = self.runCommand "empty-dot-cabal" {} ''
      mkdir -p $out/.cabal
      cat <<EOF > $out/.cabal/config
      EOF
    '';
  callCabalSdist = name: src: self.runCommand "${name}-sdist.tar.gz" {
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
  importCabal = name: src:
    # build the source dist
    let sdist = callCabalSdist name src;
    # and generate the nix expression corresponding to the source dist
    # but fixing the src to the sdist as well.
    in args: (import (callCabal2Nix sdist) args) // { src = sdist; };
in {
  # note: we want the ghc-boot-packages from
  # the *buildPackages*, as we want them from the
  # compiler we use to build this.
  ghc-boot-packages = builtins.mapAttrs (name: value:
    builtins.mapAttrs (pkgName: dir: importCabal "${name}-${pkgName}" "${value.passthru.configured-src}/${dir}") {
      ghc          = "compiler";
      ghci         = "libraries/ghci";
      ghc-boot     = "libraries/ghc-boot";
      libiserv     = "libraries/libiserv";
      iserv        = "utils/iserv";
      remote-iserv = "utils/remote-iserv";
      iserv-proxy  = "utils/iserv-proxy";
    }
  ) self.buildPackages.haskell.compiler;
}
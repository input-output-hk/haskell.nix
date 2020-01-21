self: super:
let
  emptyDotCabal = self.runCommand "empty-dot-cabal" {} ''
      mkdir -p $out/.cabal
      cat <<EOF > $out/.cabal/config
      EOF
    '';
  callCabalSdist = name: src: self.runCommand "${name}-sdist.tar.gz" {
      nativeBuildInputs = [ self.haskell-nix.cabal-install ];
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

    LOCALE_ARCHIVE = self.lib.optionalString (self.stdenv.hostPlatform.libc == "glibc") "${self.glibcLocales}/lib/locale/locale-archive";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";

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

  ghc-extra-pkgs = {
      ghc          = "compiler";
      base         = "libraries/base";
      bytestring   = "libraries/bytestring";
      ghci         = "libraries/ghci";
      ghc-boot     = "libraries/ghc-boot";
      ghc-heap     = "libraries/ghc-heap";
      libiserv     = "libraries/libiserv";
      iserv        = "utils/iserv";
      remote-iserv = "utils/remote-iserv";
      iserv-proxy  = "utils/iserv-proxy";
    };

# Given the ghc-extra-pkgs, we'll create a cabal.project
# that contains all of them.  And then we call cabalProject
# on it to generate the necessary cabal project exposing all
# the package components.
#
# The motivation here is that we can build primarily
# remote-iserv and iserv-proxy as standalone applications, as
# derived from the configured (and potentially patched) ghc
# source code.
#
# This is a bit like how we treat hsc2hs, alex, happy as external
# programs we need to build from hackage, but iserv-remote and
# iserv-proxy are not on hackage (and might have been patched)
# as part of patches we applied to the GHC tree.

in rec {
  ghc-boot-packages = builtins.mapAttrs
    (name: value: builtins.mapAttrs
      (pkgName: dir: importCabal "${name}-${pkgName}" "${value.passthru.configured-src}/${dir}") ghc-extra-pkgs)
    self.buildPackages.haskell-nix.compiler;

  ghc-extra-pkgs-cabal-projects = builtins.mapAttrs (name: value:
    let package-locs =
      builtins.mapAttrs (_: dir: "${value.passthru.configured-src}/${dir}")
        (self.lib.filterAttrs (n: _: n != "base") ghc-extra-pkgs);
    in self.writeTextFile {
      name = "ghc-extra-pkgs-cabal-project-${name}";
      destination = "/cabal.project";
      text = ''
        packages: ${self.lib.concatStringsSep " " (self.lib.attrValues package-locs)}
        -- need this for libiserve as it doesn't build against 3.0 yet.
        constraints: network < 3.0,
                     ghc +ghci,
                     ghci +ghci,
                     libiserv +network
      '';
    }) self.buildPackages.haskell-nix.compiler;

  ghc-extra-projects = builtins.mapAttrs (name: proj: self.haskell-nix.cabalProject' {
      name = "ghc-extra-packages";
      src = proj;
      index-state = "2019-10-31T00:00:00Z";
      ghc = self.buildPackages.haskell-nix.compiler.${name};
    })
    ghc-extra-pkgs-cabal-projects;

  ghc-extra-packages = builtins.mapAttrs (_: proj: proj.hsPkgs) ghc-extra-projects;
}

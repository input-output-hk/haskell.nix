{ stdenv, lib, buildPackages, mkPkgSet, nix-tools }:

let

  haskellSrc2nix = { name, src ? null, extraCabal2nixOptions ? "" }:
    buildPackages.stdenv.mkDerivation {
      name = "cabal-to-nix-${name}";
      nativeBuildInputs = [ nix-tools ];
      preferLocalBuild = true;
      allowSubstitutes = false;
      phases = ["installPhase"];
      LANG = "en_US.UTF-8";
      LOCALE_ARCHIVE = lib.optionalString (stdenv.buildPlatform.libc == "glibc") "${buildPackages.glibcLocales}/lib/locale/locale-archive";
      installPhase = ''
        export HOME="$TMP"
        mkdir -p "$out"
        cabal-to-nix "${src}/${name}.cabal" ${extraCabal2nixOptions} > "$out/default.nix"
      '';
    };

  haskellSrc2plan = { name, src, ghc }:
    buildPackages.stdenv.mkDerivation {
      name = "plan-to-nix-${name}";
      nativeBuildInputs = [ nix-tools buildPackages.cabal-install ];
      preferLocalBuild = true;
      allowSubstitutes = false;
      phases = ["installPhase"];
      LANG = "en_US.UTF-8";
      LOCALE_ARCHIVE = lib.optionalString (stdenv.buildPlatform.libc == "glibc") "${buildPackages.glibcLocales}/lib/locale/locale-archive";
      installPhase = ''
        mkdir -p "$out"

        export HOME="$TMP"
        mkdir -p "$HOME/.cabal"

        # fixme: this won't work in a sandboxed build
        cabal new-update
        # touch "$HOME/.cabal/config"

        # need to copy sources because cabal wants to write cabal.project.local
        cp -R --no-preserve=mode ${src}/* .

        cabal new-configure --builddir="$TMP/dist-newstyle" --with-compiler ${ghc}/bin/${ghc.targetPrefix}ghc
        plan-to-nix "$TMP/dist-newstyle/cache/plan.json" > "$out/default.nix"
      '';
    };

in
  name: src: ghc:
    let
      filter = path: type:
                 lib.hasSuffix "${name}.cabal" path ||
                 # baseNameOf path == "cabal.project" ||
                 baseNameOf path == "package.yaml";
      src' = if lib.canCleanSource src
              then lib.cleanSourceWith { inherit src filter; }
            else src;
      expr = haskellSrc2nix {
        inherit name;
        src = src';
      };
      plan = haskellSrc2plan {
        inherit name ghc;
        src = src';
      };
    in
      mkPkgSet {
        pkg-def = import (plan + /default.nix);
        pkg-def-overlays = [ { "${name}" = import (expr + /default.nix); } ];
        modules = [ {
          packages."${name}" = {
            src = lib.mkForce src;
            # Adds the generated nix as an input, to prevent garbage collection.
            preConfigure = "# Generated from ${expr} and ${plan}";
          };
        } ];
      }
  # fixme: allow more args for generation, etc.

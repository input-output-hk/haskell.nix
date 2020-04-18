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
  callCabal2Nix = name: src: self.stdenv.mkDerivation {
    name = "${name}-package.nix";
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

  # Combines multiple derivations into one to make them
  # easier to materialize.
  combineFiles = name: ext: files: self.stdenv.mkDerivation {
    inherit name;
    phases = [ "buildPhase" ];

    buildPhase = ''
      mkdir $out
    '' + self.lib.concatStrings (self.lib.mapAttrsToList (name: file: ''
      cp ${file} $out/${name}${ext}
    '') files);
  };

  # Calculate the `cabal sdist` of a single boot package as well as
  # the output of cabal-to-nix.
  cabalToSdistAndNix = ghcName: pkgName: src:
    # build the source dist
    let sdist = callCabalSdist "${ghcName}-${pkgName}" src;
    # and generate the nix expression corresponding to the source dist
    in {
      inherit sdist;
      nix = callCabal2Nix "${ghcName}-${pkgName}" sdist;
    };

  # Combine the all the boot package nix files for a given ghc
  # into a single derivation and materialize it.
  combineAndMaterialize = ghcName: bootPackages:
      let
        # Limiting this to "ghc865" as the not all the boot packages
        # for ghc 8.8 and above can be processed yet by `nix-tools`
        # (we need to upgrade `nix-tools` to Cabal 3 for them to work)
        skipBroken = self.lib.filterAttrs (pkgName: _:
          ghcName == "ghc865" || (pkgName != "base" && pkgName != "ghc-heap"));
        sha256 = ghc-boot-packages-nix-sha256."${ghcName}" or null;
        materializedPath = ../materialized/ghc-boot-packages-nix + "/${ghcName}";
      in (self.haskell-nix.materialize ({
          inherit sha256;
          materialized = if sha256 != null && __pathExists materializedPath
            then materializedPath
            else null;
          sha256Arg = "ghc-boot-packages-nix-sha256";
          reasonNotSafe = null;
        }) (combineFiles "${ghcName}-boot-packages-nix" ".nix" (builtins.mapAttrs
          (_: sdistAndNix: sdistAndNix.nix) (skipBroken bootPackages))));

  # Import the nix and fix the src to the sdist as well.
  importSdistAndNix = sdistAndNix:
      args: (import sdistAndNix.nix args) // { src = sdistAndNix.sdist; };

  # The packages in GHC source and the locations of them
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

  # Hashes needed to make ghc-boot-packages use fixed output derivations
  # for the nix files.
  ghc-boot-packages-nix-sha256 = {
    ghc865 = "0n5bsi67mczbwch8a3dwf58hkignb8qswv7h707dqgc43himvq3b";
    ghc882 = "1wpvy0f2m7l3h8fz0iag3q8zj6j4i7732x1f0lkxnr2whnv88dfb";
    ghc883 = "147l5a7vm8dgyb2wz692p3n3fsvq8wbjn5wnjhabdgw4mxw0c2mm";
  };

  # The nix produced by `cabalProject` differs slightly depending on
  # what the platforms are.  There are currently 3 possible outputs.
  ghc-extra-projects-type =
    if self.stdenv.hostPlatform.isWindows
      then "windows"
      else if self.stdenv.buildPlatform != self.stdenv.hostPlatform
        then "cross"
        else "default";

  # Hashes needed to make ghc-extra-projects use fixed output derivations
  # for the nix files.
  ghc-extra-projects-sha256 = {
    default = {
      ghc865 = "12mj0fmm0b0jys6pb2l7l9w2w5as5fiqkfav81mjlz9ic1rkqk92";
      ghc882 = "0q4v1iryvd5xvnkb1s02a3v6x0lj73r8bsfxrs9bxipk22bcksm9";
      ghc883 = "0dy6isqz41c0di5yf1mhyp0nbp6aw5m947b54l5074lkmc4f5alb";
    };
    windows = {
      ghc865 = "1jizwsnmdr2iglispm9wmhhbdh2gh6yfvk8x1jbj0sk9pq21xwap";
      ghc882 = "09ji5548bsqqa7iwphjxmc5i067ya6zh0v1hfls9acfrxax0sfgm";
      ghc883 = "0gliadn1nj1i5y5hjz280h1kgzql97yfh4gy63k2g7xsldmzb57b";
    };
    cross = {
      ghc865 = "10bckrm3qi1vymncjiv534b8nb8bgix8w8y91543xxwhzpmwml3f";
      ghc882 = "0hamwh6x0vfki2c4kj132x2c2hqvr1r735cfd4yrygzf86drmyf0";
      ghc883 = "0v8wq8i82c329kzi3yzv6ny63yaggbzdaaxbbklxwg16fawqn08p";
    };
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
  ghc-boot-packages-sdist-and-nix = builtins.mapAttrs
    (ghcName: value: builtins.mapAttrs
      (pkgName: dir: cabalToSdistAndNix ghcName pkgName "${value.passthru.configured-src}/${dir}") ghc-extra-pkgs)
    self.buildPackages.haskell-nix.compiler;

  # All the ghc boot package nix files for each ghc.
  ghc-boot-packages-nix = builtins.mapAttrs
    combineAndMaterialize
      ghc-boot-packages-sdist-and-nix;

  # The import nix results for each ghc boot package for each ghc (with src=sdist).
  ghc-boot-packages = builtins.mapAttrs
    (ghcName: value: builtins.mapAttrs
      (pkgName: sdistAndNix: importSdistAndNix {
        inherit (sdistAndNix) sdist;
        nix = ghc-boot-packages-nix."${ghcName}" + "/${pkgName}.nix";
      }) value)
        ghc-boot-packages-sdist-and-nix;

  # Derivation with cabal.project for use with `cabalProject'` for each ghc. 
  ghc-extra-pkgs-cabal-projects = builtins.mapAttrs (name: value:
    let package-locs =
        # TODO ghc-heap.cabal requires cabal 3.  We should update the cabalProject' call
        # in `ghc-extra-projects` below to work with this.
        (self.lib.filterAttrs (n: _: n != "base" && n != "ghc-heap") ghc-extra-pkgs);
    in self.stdenv.mkDerivation {
      name = "ghc-extra-pkgs-cabal-project-${name}";
      phases = [ "buildPhase" ];
      # Copy each cabal file from the configured ghc source and
      # add a suitable cabal.project file.
      buildPhase = ''
        ${self.lib.concatStrings (self.lib.mapAttrsToList (_: dir: ''
          mkdir -p $out/${dir}
          cp ${value.passthru.configured-src}/${dir}/*.cabal $out/${dir}
          # Remove references to libffi as the are not cross platform
          # and will break memoization (we will need to add them back)
          sed -i 's|/nix/store/.*-libffi.*/include||' $out/${dir}/*.cabal
        '') package-locs)}
        cat >$out/cabal.project <<EOF
        packages: ${self.lib.concatStringsSep " " (self.lib.attrValues package-locs)}
        -- need this for libiserve as it doesn't build against 3.0 yet.
        constraints: network < 3.0,
                     ghc +ghci,
                     ghci +ghci,
                     libiserv +network
        EOF
      '';
    }) self.buildPackages.haskell-nix.compiler;

  # A `cabalProject'` project for each ghc
  ghc-extra-projects = builtins.mapAttrs (ghcName: proj:
    # Where to look for materialization files
    let materializedPath = ../materialized/ghc-extra-projects
                             + "/${ghc-extra-projects-type}/${ghcName}";
    in self.haskell-nix.cabalProject' {
      name = "ghc-extra-projects-${ghc-extra-projects-type}-${ghcName}";
      src = proj;
      index-state = self.haskell-nix.internalHackageIndexState;
      plan-sha256 =
        ghc-extra-projects-sha256."${ghc-extra-projects-type}"."${ghcName}"
          or null;
      materialized =
        if ghc-extra-projects-sha256."${ghc-extra-projects-type}" ? "${ghcName}"
          then materializedPath
          else null;
      ghc = self.buildPackages.haskell-nix.compiler.${ghcName};
      configureArgs = "--disable-tests"; # avoid failures satisfying bytestring package tests dependencies
    })
    ghc-extra-pkgs-cabal-projects;

  # The packages from the project for each ghc
  ghc-extra-packages = builtins.mapAttrs (_: proj: proj.hsPkgs) ghc-extra-projects;
}

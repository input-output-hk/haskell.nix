{ lib, stdenv, runCommand, autoconf, automake, coreutils, findutils, jq
, haskell-nix, ghc-version, ghc-src }:

let
  varname = builtins.replaceStrings [ "-" ] [ "_" ];

  ghc-pkgs = [
    "Cabal"
    "array"
    "base"
    "binary"
    "bytestring"
    "containers"
    "deepseq"
    "directory"
    "filepath"
    "ghc-boot"
    "ghc-boot-th"
    "ghc-compact"
    "ghc-heap"
    "ghc-prim"
    "ghci"
    "integer-gmp"
    "mtl"
    "parsec"
    "pretty"
    "process"
    "rts"
    "template-haskell"
    "text"
    "time"
    "transformers"
  ] ++ lib.optionals (!stdenv.targetPlatform.isGhcjs
    || builtins.compareVersions ghc-version "9.0" > 0) [
      # GHCJS 8.10 does not have these
      "Cabal-syntax"
      "exceptions"
      "file-io"
      "ghc"
      "ghc-bignum"
      "ghc-experimental"
      "ghc-internal"
      "ghc-platform"
      "ghc-toolchain"
      "haskeline"
      "hpc"
      "libiserv"
      "os-string"
      "semaphore-compat"
      "stm"
      "xhtml"
    ] ++ lib.optionals (!stdenv.targetPlatform.isGhcjs) [ "terminfo" ]
    ++ (if stdenv.targetPlatform.isWindows then [ "Win32" ] else [ "unix" ]);

  configure = ghc-src:
    let
      ghc-m4-src = builtins.path {
        filter = path: type: lib.hasPrefix "${ghc-src}/m4" path;
        path = ghc-src;
        name = "ghc-src-m4";
      };
    in runCommand "configure" {
      nativeBuildInputs = [ autoconf automake coreutils findutils ];
    } ''
      set -euo pipefail

      mkdir -p $out
      cd $out

      cp -v ${./configure.ac} configure.ac
      aclocal --verbose -I "${ghc-m4-src}/m4"
      autoconf --verbose 
    '';

  x = ghc-src:
    let
      ## GHC version
      flags = (lib.optional stdenv.targetPlatform.isx86_64 "--arch x86_64")
        ++ (lib.optional stdenv.targetPlatform.isAarch64 "--arch aarch64")
        ++ (lib.optional stdenv.targetPlatform.isLinux "--os linux")
        ++ (lib.optional stdenv.targetPlatform.isDarwin "--os osx")
        ++ (lib.optional stdenv.targetPlatform.isGhcjs "--arch javascript");

    in runCommand "x" {
      nativeBuildInputs = [
        # FIXME: for testing
        haskell-nix.nix-tools.exes.cabal2json
        # haskell-nix.nix-tools-unchecked.exes.cabal2json
        jq
      ];
    } ''
      set -euo pipefail
      shopt -s globstar

      mkdir -p $out
      cd $out

      # Run configure script (only for the project version)
      ${configure ghc-src}/configure --srcdir=${ghc-src}

      # Copy the cabal.in files
      pushd ${ghc-src}
      cp --no-preserve all --parents */*.cabal */*.cabal.in libraries/*/*.cabal libraries/*/*.cabal.in "$out"
      popd

      find

      # Substitute config variables in the cabal files
      FILES=( **/*.cabal.in )
      ./config.status $(printf -- '--file %s ' ''${FILES[@]%.in})

      FLAGS=(${lib.concatStringsSep " " flags})
      VERSION=$(cabal2json  ghc/ghc-bin.cabal | jq .version)
      FLAGS+=(--compiler ghc-$VERSION)

      # Convert to json
      for f in **/*.cabal; do
        # FIXME
        if [[ $f == rts/rts.cabal ]]; then
          continue
        fi
        cabal2json ${lib.concatStringsSep " " flags} --out="$f.json" "$f"
      done
    '';

in runCommand "dummy-ghc-pkg-dump" {
  nativeBuildInputs = [ haskell-nix.nix-tools-unchecked.exes.cabal2json jq ];
  passthru = { inherit configure x; };
} ''
  PACKAGE_VERSION=${ghc-version}
  ProjectVersion=${ghc-version}

  # The following logic is from GHC m4/setup_project_version.m4

  # Split PACKAGE_VERSION into (possibly empty) parts
  VERSION_MAJOR=`echo $PACKAGE_VERSION | sed 's/^\([^.]*\)\(\.\{0,1\}\(.*\)\)$/\1'/`
  VERSION_TMP=`echo $PACKAGE_VERSION | sed 's/^\([^.]*\)\(\.\{0,1\}\(.*\)\)$/\3'/`
  VERSION_MINOR=`echo $VERSION_TMP | sed 's/^\([^.]*\)\(\.\{0,1\}\(.*\)\)$/\1'/`
  ProjectPatchLevel=`echo $VERSION_TMP | sed 's/^\([^.]*\)\(\.\{0,1\}\(.*\)\)$/\3'/`

  # Calculate project version as an integer, using 2 digits for minor version
  case $VERSION_MINOR in
    ?) ProjectVersionInt=''${VERSION_MAJOR}0''${VERSION_MINOR} ;;
    ??) ProjectVersionInt=''${VERSION_MAJOR}''${VERSION_MINOR} ;;
    *) echo bad minor version in $PACKAGE_VERSION; exit 1 ;;
  esac
  # AC_SUBST([ProjectVersionInt])

  # The project patchlevel is zero unless stated otherwise
  test -z "$ProjectPatchLevel" && ProjectPatchLevel=0

  # AC_SUBST([ProjectPatchLevel1])
  # AC_SUBST([ProjectPatchLevel2])

  # Remove dots from the patch level; this allows us to have versions like 6.4.1.20050508
  ProjectPatchLevel=`echo $ProjectPatchLevel | sed 's/\.//'`

  # AC_SUBST([ProjectPatchLevel])

  # The version of the GHC package changes every day, since the
  # patchlevel is the current date.  We don't want to force
  # recompilation of the entire compiler when this happens, so for
  # GHC HEAD we omit the patchlevel from the package version number.
  #
  # The ProjectPatchLevel1 > 20000000 iff GHC HEAD. If it's for a stable
  # release like 7.10.1 or for a release candidate such as 7.10.1.20141224
  # then we don't omit the patchlevel components.

  ProjectVersionMunged="$ProjectVersion"
  if test "$ProjectPatchLevel1" -gt 20000000; then
    ProjectVersionMunged="''${VERSION_MAJOR}.''${VERSION_MINOR}"
  fi
  # AC_SUBST([ProjectVersionMunged])

  # The version used for libraries tightly coupled with GHC (e.g.
  # ghc-internal) which need a major version bump for every minor/patchlevel
  # GHC version.
  # Example: for GHC=9.10.1, ProjectVersionForLib=9.1001
  #
  # Just like with project version munged, we don't want to use the
  # patchlevel version which changes every day, so if using GHC HEAD, the
  # patchlevel = 00.
  case $VERSION_MINOR in
    ?) ProjectVersionForLibUpperHalf=''${VERSION_MAJOR}.0''${VERSION_MINOR} ;;
    ??) ProjectVersionForLibUpperHalf=''${VERSION_MAJOR}.''${VERSION_MINOR} ;;
    *) echo bad minor version in $PACKAGE_VERSION; exit 1 ;;
  esac

  # Save split version of ProjectPatchLevel
  ProjectPatchLevel1=`echo $ProjectPatchLevel | sed 's/^\([^.]*\)\(\.\{0,1\}\(.*\)\)$/\1/'`
  ProjectPatchLevel2=`echo $ProjectPatchLevel | sed 's/^\([^.]*\)\(\.\{0,1\}\(.*\)\)$/\3/'`

  # The project patchlevel1/2 is zero unless stated otherwise
  test -z "$ProjectPatchLevel1" && ProjectPatchLevel1=0
  test -z "$ProjectPatchLevel2" && ProjectPatchLevel2=0

  # GHC HEAD uses patch level version > 20000000
  case $ProjectPatchLevel1 in
    ?) ProjectVersionForLib=''${ProjectVersionForLibUpperHalf}0''${ProjectPatchLevel1} ;;
    ??) ProjectVersionForLib=''${ProjectVersionForLibUpperHalf}''${ProjectPatchLevel1} ;;
    *) ProjectVersionForLib=''${ProjectVersionForLibUpperHalf}00
  esac

  PKGS=""
  ${lib.concatStrings (builtins.map (name: ''
    cabal_file=""
    if [ -f ${ghc-src}/libraries/${name}/${name}.cabal ]; then
      cabal_file=${ghc-src}/libraries/${name}/${name}.cabal
    elif [ -f ${ghc-src}/libraries/Cabal/${name}/${name}.cabal ]; then
      cabal_file=${ghc-src}/libraries/Cabal/${name}/${name}.cabal
    elif [ -f ${ghc-src}/libraries/${name}/${name}/${name}.cabal ]; then
      cabal_file=${ghc-src}/libraries/${name}/${name}/${name}.cabal
    elif [ -f ${ghc-src}/compiler/${name}.cabal ]; then
      cabal_file=${ghc-src}/compiler/${name}.cabal
    elif [ -f ${ghc-src}/compiler/${name}.cabal.in ]; then
      cabal_file=${ghc-src}/compiler/${name}.cabal.in
    elif [ -f ${ghc-src}/libraries/${name}/${name}.cabal.in ]; then
      cabal_file=${ghc-src}/libraries/${name}/${name}.cabal.in
    fi
    if [[ "$cabal_file" != "" ]]; then
      fixed_cabal_file=$(mktemp)
      cat $cabal_file | sed -e "s/@ProjectVersionMunged@/$ProjectVersionMunged/g" -e "s/@ProjectVersionForLib@/$ProjectVersionForLib/g" -e 's/default: *@[A-Za-z0-9]*@/default: False/g' -e 's/@Suffix@//g' > $fixed_cabal_file
      json_cabal_file=$(mktemp)
      cabal2json $fixed_cabal_file > $json_cabal_file

      exposed_modules="$(jq -r '.library."exposed-modules"[]|select(type=="array")[]' $json_cabal_file)"
      reexported_modules="$(jq -r '.library."reexported-modules"//[]|.[]|select(type=="array")[]' $json_cabal_file)"

      # FIXME This is a bandaid. Rather than doing this, conditionals should be interpreted.
      ${
        lib.optionalString stdenv.targetPlatform.isGhcjs ''
          exposed_modules+=" $(jq -r '.library."exposed-modules"[]|select(type=="object" and .if.arch == "javascript")|.then[]' $json_cabal_file)"
        ''
      }
      ${
        lib.optionalString stdenv.targetPlatform.isWindows ''
          exposed_modules+=" $(jq -r '.library."exposed-modules"[]|select(type=="object" and .if.os == "windows")|.then[]' $json_cabal_file)"
        ''
      }
      ${
        lib.optionalString (!stdenv.targetPlatform.isWindows) ''
          exposed_modules+=" $(jq -r '.library."exposed-modules"[]|select(type=="object" and .if.not.os == "windows")|.then[]' $json_cabal_file)"
        ''
      }

      EXPOSED_MODULES_${
        varname name
      }="$(tr '\n' ' ' <<< "$exposed_modules $reexported_modules")"
      DEPS_${
        varname name
      }="$(jq -r '.library."build-depends"[]|select(type=="array")[],select(type=="object" and .if.not.flag != "vendor-filepath").then[]' $json_cabal_file | sed 's/^\([A-Za-z0-9-]*\).*$/\1/g' | sort -u | tr '\n' ' ')"
      VER_${varname name}="$(jq -r '.version' $json_cabal_file)"
      PKGS+=" ${name}"
      LAST_PKG="${name}"
    fi
  '') ghc-pkgs)}

  ${ # There is no .cabal file for system-cxx-std-lib
  lib.optionalString (builtins.compareVersions ghc-version "9.2" >= 0)
  (let name = "system-cxx-std-lib";
  in ''
    EXPOSED_MODULES_${varname name}=""
    DEPS_${varname name}=""
    VER_${varname name}="1.0"
    PKGS+=" ${name}"
    LAST_PKG="${name}"
  '')
  # ghcjs packages (before the ghc JS backend). TODO remove this when GHC 8.10 support is dropped
  + lib.optionalString (stdenv.targetPlatform.isGhcjs
    && builtins.compareVersions ghc-version "9" < 0) ''
      EXPOSED_MODULES_${
        varname "ghcjs-prim"
      }="GHCJS.Prim GHCJS.Prim.Internal GHCJS.Prim.Internal.Build"
      DEPS_${varname "ghcjs-prim"}="base ghc-prim"
      VER_${varname "ghcjs-prim"}="0.1.1.0"
      EXPOSED_MODULES_${
        varname "ghcjs-th"
      }="GHCJS.Prim.TH.Eval GHCJS.Prim.TH.Types"
      DEPS_${
        varname "ghcjs-th"
      }="base binary bytestring containers ghc-prim ghci template-haskell"
      VER_${varname "ghcjs-th"}="0.1.0.0"
      PKGS+=" ghcjs-prim ghcjs-th"
      LAST_PKG="ghcjs-th"
    ''}
  for pkg in $PKGS; do
    varname="$(echo $pkg | tr "-" "_")"
    ver="VER_$varname"
    exposed_mods="EXPOSED_MODULES_$varname"
    deps="DEPS_$varname"
    echo "name: $pkg" >> $out
    echo "version: ''${!ver}" >> $out
    echo "exposed-modules: ''${!exposed_mods}" >> $out
    echo "depends:" >> $out
    for dep in ''${!deps}; do
      ver_dep="VER_$(echo $dep | tr "-" "_")"
      if [[ "''${!ver_dep}" != "" ]]; then
        echo "  $dep-''${!ver_dep}" >> $out
      fi
    done
    if [[ "$pkg" != "$LAST_PKG" ]]; then
      echo '---' >> $out
    fi
  done
''

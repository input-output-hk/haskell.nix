{
  autoconf,
  coreutils,
  findutils,
  haskell-nix,
  jq,
  lib,
  runCommand,
  stdenv,
  writeText,
}:

let
  inherit (haskell-nix.nix-tools-unchecked.exes) cabal2json;
in

{ ghc-src, ghc-version }:

let
  ghc-m4-src = builtins.path {
    filter = path: type: lib.hasPrefix "${ghc-src}/m4" path;
    path = ghc-src;
    name = "ghc-src-m4";
  };

  configure-ac = writeText "configure.ac" ''
    m4_include([fp_check_prog.m4])
    m4_include([fp_prog_find.m4])
    m4_include([fp_prog_sort.m4])
    m4_include([fp_setup_project_version.m4])

    AC_INIT([The Glorious Glasgow Haskell Compilation System],
            [${ghc-version}],
    		[glasgow-haskell-bugs@haskell.org],
    		[ghc])

    # Set this to YES for a released version, otherwise NO
    : ''${RELEASE=NO}

    dnl ** Make sure we do not have a bogus source directory
    AC_CONFIG_SRCDIR(ghc/Main.hs)

    dnl ----------------------------------------------------------
    dnl ** Find unixy sort and find commands,
    dnl ** which are needed by FP_SETUP_PROJECT_VERSION

    FP_PROG_FIND
    FP_PROG_SORT

    dnl ----------------------------------------------------------
    pushd ''${srcdir}
    FP_SETUP_PROJECT_VERSION
    popd

    dnl ** Output code to perform substitutions
    AC_CONFIG_FILES

    AC_OUTPUT
  '';

  configure =
    runCommand "dummy-ghc-configure"
      {
        nativeBuildInputs = [
          autoconf
          coreutils
          findutils
        ];
      }
      ''
        autoconf --warnings all --include "${ghc-m4-src}/m4" --output=$out ${configure-ac}
      '';

  cabal2json-flags =
    with stdenv.targetPlatform;
    lib.cli.toGNUCommandLineShell { } {
      arch =
        lib.optional isx86_64 "x86_64"
        ++ lib.optional isAarch64 "aarch64"
        ++ lib.optional isGhcjs "javascript";
      os = lib.optional isLinux "linux" ++ lib.optional isDarwin "osx";
    };

  cabal-files =
    runCommand "dummy-ghc-cabal-files"
      {
        nativeBuildInputs = [ cabal2json ];
      }
      ''
        set -euo pipefail
        shopt -s globstar

        mkdir -p $out
        cd $out

        # Run configure script (only for the project version)
        ${configure} --srcdir=${ghc-src}

        # Copy the cabal.in files
        pushd ${ghc-src}
        cp --no-preserve all --parents */*.cabal */*.cabal.in libraries/*/*.cabal libraries/*/*.cabal.in "$out"
        popd

        # Substitute config variables in the cabal files
        CABAL_IN=( **/*.cabal.in )
        ./config.status $(printf -- '--file %s ' "''${CABAL_IN[@]%.in}")

        # Convert to json
        for f in **/*.cabal; do
          # FIXME
          if [[ $f == rts/rts.cabal ]]; then
            continue
          fi
          cabal2json ${cabal2json-flags} --compiler ghc-${ghc-version} --out="$f.json" "$f"
        done
      '';

in
runCommand "dummy-ghc-pkg-dump" { } ''
  set -eo pipefail
  shopt -s globstar
  ${lib.getExe jq} --null-input --raw-output --from-file ${./script.jq} ${cabal-files}/**/*.cabal.json >$out
''

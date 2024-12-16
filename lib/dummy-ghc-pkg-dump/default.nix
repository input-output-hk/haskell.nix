{
  autoconf,
  cabal2json,
  coreutils,
  findutils,
  ghc-src,
  ghc-version,
  jq,
  lib,
  runCommand,
  stdenv,
}:

let
  ghc-m4-src = builtins.path {
    filter = path: type: lib.hasPrefix "${ghc-src}/m4" path;
    path = ghc-src;
    name = "ghc-src-m4";
  };

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
        set -euo pipefail
        autoconf --warnings all --include "${ghc-m4-src}/m4" --output=$out ${./configure.ac}
      '';

  # cabal2json = builtins.fetchClosure {
  #   fromStore = "https://cache.zw3rk.com";
  #   fromPath = /nix/store/q1kqwvqmwqcqaq82gaf951mq39v4sc91-Cabal-syntax-json-exe-cabal2json-0.1.0.0;
  # };

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
        ${configure} --srcdir=${ghc-src} PACKAGE_VERSION=${ghc-version}

        # Copy the cabal.in files
        pushd ${ghc-src}
        cp --no-preserve all --parents */*.cabal */*.cabal.in libraries/*/*.cabal libraries/*/*.cabal.in "$out"
        popd

        # Substitute config variables in the cabal files
        FILES=( **/*.cabal.in )
        ./config.status $(printf -- '--file %s ' "''${FILES[@]%.in}")

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

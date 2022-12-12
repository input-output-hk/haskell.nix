{ stdenv, lib, writeScript, diffutils }:

with lib;

writeScript "check-materialization-concurrency.sh" ''
  #!${stdenv.shell}

  set -euo pipefail

  WORK=$(mktemp -d)

  # We expect to see both the hello and world derivations to start building
  # before either of them finish.
  echo EVENT start hello > $WORK/expected.txt
  echo EVENT start world >> $WORK/expected.txt
  echo EVENT end hello >> $WORK/expected.txt
  echo EVENT end world >> $WORK/expected.txt

  nix-build -j2 scripts/check-materialization-concurrency --arg n "\"$(date)\"" 2>&1 | grep '^EVENT' > $WORK/actual.txt

  ${diffutils}/bin/diff -u $WORK/expected.txt $WORK/actual.txt
''


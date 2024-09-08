{ stdenv, glibc, lib, writeScript, coreutils, time, gnutar, gzip, hydra_unstable, jq, gitMinimal }:

with lib;

writeScript "check-hydra.sh" ''
  #! /usr/bin/env nix-shell
  #! nix-shell -i bash --pure -p glibc coreutils time gnutar gzip hydra_unstable jq gitMinimal

  set -euo pipefail

  export NIX_PATH=

  echo '~~~ Evaluating release.nix with' "$@"
  HYDRA_CONFIG= command time --format '%e' -o eval-time.txt \
      hydra-eval-jobs \
      --flake $(pwd) > eval.json
  EVAL_EXIT_CODE="$?"
  if [ "$EVAL_EXIT_CODE" != 0 ]
  then
    rm eval.json eval-time.txt
    echo -e "\\e[31;1mERROR: Failed to evaluate release.nix\\e[0m"
    exit 1
  fi
  EVAL_TIME=$(cat eval-time.txt)
  jq . < eval.json
  ERRORS=$(jq -r 'map_values(.error)|to_entries[]|select(.value)|@text "\(.key): \(.value)"' < eval.json)
  NUM_ERRORS=$(jq -r '[ map_values(.error)|to_entries[]|select(.value) ] |length' < eval.json)
  rm eval.json eval-time.txt

  if [ "$NUM_ERRORS" != 0 ]
  then
    echo -e "\\e[31;1mERROR: evaluation completed in $EVAL_TIME seconds with $NUM_ERRORS errors\\e[0m"
    echo "$ERRORS"
    exit 1
  else
    echo -e "\\e[32;1mOK: evaluation completed in $EVAL_TIME seconds with no errors\\e[0m"
    exit 0
  fi
''

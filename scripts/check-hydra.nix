{ stdenv, lib, writeScript, coreutils, time, gnutar, gzip, hydra-unstable, jq, gitMinimal }:

with lib;

writeScript "check-hydra.sh" ''
  #!${stdenv.shell}

  set -euo pipefail

  export PATH="${makeBinPath [ coreutils time gnutar gzip hydra-unstable jq gitMinimal ]}"
  export NIX_PATH=

  echo '~~~ Evaluating release.nix with' "$@"
  command time --format '%e' -o eval-time.txt \
      hydra-eval-jobs \
      --option allowed-uris "https://github.com/NixOS https://github.com/input-output-hk" \
      --arg supportedSystems '[ builtins.currentSystem ]' \
      "$@" \
      -I $(realpath .) release.nix > eval.json
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

{ stdenv, writeScript, coreutils, gawk, nix
, nix-tools
, limitMB ? 525
}:

with stdenv.lib;

writeScript "check-closure-size.sh" ''
  #!${stdenv.shell}

  set -euo pipefail

  export PATH="${makeBinPath [ coreutils gawk nix ]}"

  get_closure_size() {
    du -scm $(nix-store -qR $1) | sort -n | tail -n25
  }

  nt="$(get_closure_size ${nix-tools})"
  echo '	${nix-tools}'
  echo "$nt"

  total=$(awk '$2 == "total" { print $1; }' <<< "$nt")
  
  if [ $total -gt ${toString limitMB} ]; then
    echo "Closure size exceeds limit of ${toString limitMB}MB!"
    exit 1
  fi
''

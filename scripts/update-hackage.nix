{ stdenv, writeScriptBin, coreutils, git, nix-tools, cabal-install, nix-prefetch-git }:

with stdenv.lib;

writeScriptBin "update-hackage-nix" ''
  #!${stdenv.shell}

  set -euo pipefail

  export PATH="${makeBinPath [ coreutils git nix-tools cabal-install nix-prefetch-git ]}"

  # Make sure the hackage index is recent.
  cabal new-update

  if [ -d hackage.nix ]; then
    cd hackage.nix
    git pull --ff-only
    cd ..
  else
    git clone git@github.com:input-output-hk/hackage.nix.git
  fi

  hackage-to-nix hackage.nix

  cd hackage.nix
  git add .
  git commit --allow-empty -m "Automatic update for $(date)"

  rev=$(git rev-parse HEAD)

  git push

  cd ..

  nix-prefetch-git https://github.com/input-output-hk/hackage.nix.git --rev "$rev" | tee hackage-src.json
''

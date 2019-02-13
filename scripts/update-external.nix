{ stdenv, writeScript, glibc, coreutils, git, nix-tools, cabal-install, nix-prefetch-git }:

{ name, script }:

with stdenv.lib;

writeScript "update-${name}-nix.sh" ''
  #!${stdenv.shell}

  set -euo pipefail

  export PATH="${makeBinPath [ coreutils glibc git nix-tools cabal-install nix-prefetch-git ]}"

  ${script}

  git add .
  git commit --allow-empty -m "Automatic update for $(date)"

  rev=$(git rev-parse HEAD)

  git push

  cd ..

  nix-prefetch-git https://github.com/input-output-hk/${name}.nix.git --rev "$rev" | tee ${name}-src.json
''

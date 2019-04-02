{ stdenv, writeScript, glibc, coreutils, git, openssh
, nix-tools, cabal-install, nix-prefetch-git }:

{ name, script }:

with stdenv.lib;

let
  repoHTTPS = "https://github.com/input-output-hk/${name}.nix";
  repoSSH = "git@github.com:input-output-hk/${name}.nix.git";
  sshKey = "/run/keys/buildkite-${name}-ssh-private";
in
  writeScript "update-${name}-nix.sh" ''
    #!${stdenv.shell}

    set -euo pipefail

    export PATH="${makeBinPath [ coreutils glibc git openssh nix-tools cabal-install nix-prefetch-git ]}"

    ${script}

    source ${./git.env}

    echo "Committing changes..."
    git add .
    check_staged
    git commit --message "Automatic update for $(date)"

    use_ssh_key ${sshKey}

    git push ${repoSSH}

    rev=$(git rev-parse HEAD)

    cd ..

    nix-prefetch-git ${repoHTTPS} --rev "$rev" | tee ${name}-src.json
  ''

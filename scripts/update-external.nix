{ stdenv, lib, writeScript, glibc, coreutils, git, openssh
, nix-tools, nixFlakes
, bash, curl, findutils, gawk, cabal-issue-8352-workaround }:

{ name, script }:

with lib;

let
  repoSSH = "git@github.com:input-output-hk/${name}.nix.git";
  sshKey = "/run/keys/buildkite-${name}-ssh-private";
in
  writeScript "update-${name}-nix.sh" ''
    #!${stdenv.shell}

    set -euo pipefail

    export PATH="${makeBinPath ([ coreutils curl findutils gawk bash git openssh nix-tools nixFlakes ] ++ cabal-issue-8352-workaround ++ optional stdenv.isLinux glibc)}"

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

    nix flake lock --accept-flake-config \
                   --experimental-features 'nix-command flakes' \
                   --update-input ${name}
  ''

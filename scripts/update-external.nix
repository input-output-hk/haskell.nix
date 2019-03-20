{ stdenv, writeScript, glibc, coreutils, git, openssh
, nix-tools, cabal-install, nix-prefetch-git }:

{ name, script }:

with stdenv.lib;

let
  repoHTTPS = "https://github.com/input-output-hk/${name}.nix.git";
  repoSSH = "git@github.com:input-output-hk/${name}.nix.git";
  sshKey = "/run/keys/buildkite-${name}-ssh-private";
in
  writeScript "update-${name}-nix.sh" ''
    #!${stdenv.shell}

    set -euo pipefail

    export PATH="${makeBinPath [ coreutils glibc git openssh nix-tools cabal-install nix-prefetch-git ]}"

    ${script}

    echo "Committing changes..."
    export GIT_COMMITTER_NAME="IOHK"
    export GIT_COMMITTER_EMAIL="devops+nix-tools@iohk.io"
    export GIT_AUTHOR_NAME="$GIT_COMMITTER_NAME"
    export GIT_AUTHOR_EMAIL="$GIT_COMMITTER_EMAIL"
    git add .
    git commit --allow-empty --message "Automatic update for $(date)"

    rev=$(git rev-parse HEAD)

    if [ -e ${sshKey} ]
    then
      echo "Authenticating using SSH with ${sshKey}"
      export GIT_SSH_COMMAND="ssh -i ${sshKey} -F /dev/null"
    else
      echo "There is no SSH key at ${sshKey}"
      echo "Git push may not work."
    fi

    git push ${repoSSH}

    cd ..

    nix-prefetch-git ${repoHTTPS} --rev "$rev" | tee ${name}-src.json
  ''

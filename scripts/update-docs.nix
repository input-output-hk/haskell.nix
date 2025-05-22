{ stdenv, lib, writeScript, coreutils, glibc, git, openssh, gnused, mdbook
, generatedOptions }:

with lib;

let
  repo = "git@github.com:input-output-hk/haskell.nix.git";
  sshKey = "/run/keys/buildkite-haskell-dot-nix-ssh-private";
in
  # update-docs depends on glibc which doesn't build on darwin
  meta.addMetaAttrs { platforms = platforms.linux; } (writeScript "update-docs.sh" ''
    #!${stdenv.shell}

    set -euo pipefail

    export PATH="${makeBinPath [ coreutils glibc git openssh gnused mdbook ]}"

    source ${./git.env}

    rev=$(git rev-parse --short HEAD)
    cd $(git rev-parse --show-toplevel)

    echo "Preprocessing..."
    cat ${generatedOptions} > docs/reference/modules.md

    echo "Building..."
    rm -rf book
    mdbook build
    touch book/.nojekyll
    rm docs/reference/modules.md

    echo "Updating git index..."
    git fetch origin
    git checkout gh-pages
    git reset --hard origin/gh-pages
    GIT_WORK_TREE=$(pwd)/book git add -A
    check_staged
    echo "Committing changes..."
    git commit --no-gpg-sign --message "Update gh-pages for $rev"

    use_ssh_key ${sshKey}

    if [ "''${GITHUB_REF:-}" = refs/heads/master ]; then
      git push ${repo} HEAD:gh-pages
    fi
  '')

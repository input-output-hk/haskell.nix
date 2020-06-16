{ stdenv, writeScript, coreutils, glibc, git, openssh, gnused, mkdocs
, generatedOptions }:

with stdenv.lib;

let
  repo = "git@github.com:input-output-hk/haskell.nix.git";
  sshKey = "/run/keys/buildkite-haskell-dot-nix-ssh-private";
in
  # update-docs depends on glibc which doesn't build on darwin
  meta.addMetaAttrs { platforms = platforms.linux; } (writeScript "update-docs.sh" ''
    #!${stdenv.shell}

    set -euo pipefail

    export PATH="${makeBinPath [ coreutils glibc git openssh gnused mkdocs ]}"

    source ${./git.env}

    rev=$(git rev-parse --short HEAD)
    cd $(git rev-parse --show-toplevel)

    echo "Preprocessing..."
    cat ${generatedOptions} > docs/reference/modules.md

    echo "Building..."
    rm -rf site
    mkdocs build
    touch site/.nojekyll
    sed -i -e '/Build Date/d' site/index.html
    sed -i -e '/lastmod/d' site/sitemap.xml
    rm -f site/sitemap.xml.gz
    rm docs/reference/modules.md

    echo "Updating git index..."
    git fetch origin
    git checkout gh-pages
    git reset --hard origin/gh-pages
    GIT_WORK_TREE=$(pwd)/site git add -A
    check_staged
    echo "Committing changes..."
    git commit --no-gpg-sign --message "Update gh-pages for $rev"

    use_ssh_key ${sshKey}

    if [ "''${BUILDKITE_BRANCH:-}" = master ]; then
      git push ${repo} HEAD:gh-pages
    fi
  '')

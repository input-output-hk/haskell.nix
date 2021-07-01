{ stdenv, lib, writeScript, coreutils, glibc, git, openssh
, nix-tools, cabal-install, nix-prefetch-git
, gawk, bash, curl, findutils }@args:

import ./update-external.nix args {
  name = "stackage";
  script = ''
    # Clone or update the main Stackage Nix expressions repo.
    # The upstream LTS and Nightly package sets are in submodules, which
    # should also be updated.
    if [ -d stackage.nix ]; then
      cd stackage.nix
      git pull --ff-only
      git submodule update --init
      git submodule foreach git pull origin master
    else
      git clone git@github.com:input-output-hk/stackage.nix.git
      cd stackage.nix
      git submodule update --init
      git submodule foreach git pull origin master
    fi

    echo "Running lts-to-nix for all snapshots..."

    LTS_TO_NIX=lts-to-nix ./update.sh
  '';
}

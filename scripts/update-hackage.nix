{ stdenv, writeScript, coreutils, glibc, git, openssh
, nix-tools, cabal-install, nix-prefetch-git
, gawk, bash, curl, findutils
, compiler-nix-name
, update-index-state-hashes }@args:

import ./update-external.nix
(removeAttrs args ["update-index-state-hashes"]) {
  name = "hackage";
  script = ''
    # Clone or update the Hackage Nix expressions repo.
    if [ -d hackage.nix ]; then
      cd hackage.nix
      git pull --ff-only
      cd ..
    else
      git clone git@github.com:input-output-hk/hackage.nix.git
    fi

    set -x
    # Make sure the hackage index is recent.
    echo "Updating local hackage index..."
    cabal update

    echo "Running hackage-to-nix..."

    hackage-to-nix hackage.nix

    echo "Running update-index-state-hashes..."

    cd hackage.nix

    ${update-index-state-hashes compiler-nix-name}/bin/update-index-state-hashes > index-state-hashes.nix
  '';
}

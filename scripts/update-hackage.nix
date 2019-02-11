{ stdenv, writeScript, coreutils, glibc, git, nix-tools, cabal-install, nix-prefetch-git }@args:

import ./update-external.nix args {
  name = "hackage";
  script = ''
    # Make sure the hackage index is recent.
    cabal new-update

    # Clone or update the Hackage Nix expressions repo.
    if [ -d hackage.nix ]; then
      cd hackage.nix
      git pull --ff-only
      cd ..
    else
      git clone git@github.com:input-output-hk/hackage.nix.git
    fi

    echo "Running hackage-to-nix..."

    hackage-to-nix hackage.nix

    cd hackage.nix
  '';
}

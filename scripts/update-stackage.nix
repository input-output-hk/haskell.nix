{ stdenv, writeScript, coreutils, glibc, git, openssh, nix-tools, cabal-install, nix-prefetch-git }@args:

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

    for lts in {lts-haskell,stackage-nightly}/*.yaml
    do
      if [[ ! -f $(basename ''${lts%.yaml}.nix) ]]; then
        lts-to-nix $lts > $(basename ''${lts%.yaml}.nix)
      fi
    done
    
    # update nightlies
    echo "{" > nightlies.nix;
    for a in nightly-*.nix; do echo "  \"''${a%%.nix}\" = import ./$a;" >> nightlies.nix; done;
    echo "}" >> nightlies.nix
    # update lts
    echo "{" > ltss.nix;
    for a in $(ls lts-*.nix | sort -Vtx -k 1,1); do echo "  \"''${a%%.nix}\" = import ./$a;" >> ltss.nix; done;
    echo "}" >> ltss.nix
  '';
}

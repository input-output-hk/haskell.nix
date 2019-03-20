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
    fi

    echo "Running lts-to-nix for all snapshots..."

    # update them all in parallel...
    N=$(getconf _NPROCESSORS_ONLN)
    for lts in {lts-haskell,stackage-nightly}/*.yaml
    do
        lts-to-nix $lts > $(basename ''${lts%.yaml}.nix) &
        while [[ $(jobs -r -p | wc -l) -gt $N ]]; do
        # can't use `wait -n` on older bash versions.
        # e.g. what ships with macOS High Sierra
        sleep 1;
        done
    done
    wait

    # update nightlies
    echo "{" > nightlies.nix;
    for a in nightly-*.nix; do echo "  \"''${a%%.nix}\" = import ./$a;" >> nightlies.nix; done;
    echo "}" >> nightlies.nix
    # update lts
    echo "{" > ltss.nix;
    for a in lts-*.nix; do echo "  \"''${a%%.nix}\" = import ./$a;" >> ltss.nix; done;
    echo "}" >> ltss.nix
  '';
}

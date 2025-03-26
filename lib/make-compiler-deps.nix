{ lib, runCommand }:
ghc: ghc // {
    cachedDeps = runCommand "${ghc.name}-deps" {}
      # First checks that ghc-pkg runs first with `--version` as failures in the `for` and
      # `if` statements will be masked.
      ''
      mkdir $out
      ${ghc}/bin/${ghc.targetPrefix}ghc-pkg --version
      for P in $(${ghc}/bin/${ghc.targetPrefix}ghc-pkg list --simple-output | sed 's/-[0-9][0-9.]*//g'); do
        mkdir -p $out/exactDeps/$P
        touch $out/exactDeps/$P/configure-flags
        touch $out/exactDeps/$P/cabal.config

        if id=$(${ghc}/bin/${ghc.targetPrefix}ghc-pkg field $P id --simple-output); then
          echo "--dependency=$P=$id" >> $out/exactDeps/$P/configure-flags
        elif id=$(${ghc}/bin/${ghc.targetPrefix}ghc-pkg field "z-$P-z-*" id --simple-output); then
          name=$(${ghc}/bin/${ghc.targetPrefix}ghc-pkg field "z-$P-z-*" name --simple-output)
          # so we are dealing with a sublib. As we build sublibs separately, the above
          # query should be safe.
          echo "--dependency=''${name#z-$P-z-}=$id" >> $out/exactDeps/$P/configure-flags
        fi
        if ver=$(${ghc}/bin/${ghc.targetPrefix}ghc-pkg field $P version --simple-output); then
          echo "constraint: $P == $ver" >> $out/exactDeps/$P/cabal.config
          echo "constraint: $P installed" >> $out/exactDeps/$P/cabal.config
        fi
      done

      mkdir -p $out/envDeps
      for P in $(${ghc}/bin/${ghc.targetPrefix}ghc-pkg list --simple-output | sed 's/-[0-9][0-9.]*//g'); do
        touch $out/envDeps/$P
        if id=$(${ghc}/bin/${ghc.targetPrefix}ghc-pkg field $P id --simple-output); then
          echo "package-id $id" >> $out/envDeps/$P
        fi
      done
      '';
  }

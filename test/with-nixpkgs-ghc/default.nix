{ with-nixpkgs ? true }:
let

  haskell-nix = import ../../default.nix { };
  nixpkgs = haskell-nix.inputs.nixpkgs.outPath;

  fix = final: prev: compiler-nix-name:
    let
      haskell-nix-ghc = prev.haskell-nix.compiler.${compiler-nix-name};
      nixpkgs-ghc = prev.haskell.compiler.${compiler-nix-name};

      exactDepOut =
        pkgs.runCommand "${nixpkgs-ghc.name}-exactDeps" { }
          ''
            mkdir -p $out
            ${nixpkgs-ghc}/bin/ghc-pkg --version
            for P in $(${nixpkgs-ghc}/bin/ghc-pkg list --simple-output | sed 's/-[0-9][0-9.]*//g'); do
              mkdir -p $out/exactDeps/$P
              touch $out/exactDeps/$P/configure-flags
              touch $out/exactDeps/$P/cabal.config

              if id=$(${nixpkgs-ghc}/bin/ghc-pkg field $P id --simple-output); then
                echo "--dependency=$P=$id" >> $out/exactDeps/$P/configure-flags
              elif id=$(${nixpkgs-ghc}/bin/ghc-pkg field "z-$P-z-*" id --simple-output); then
                name=$(${nixpkgs-ghc}/bin/ghc-pkg field "z-$P-z-*" name --simple-output)
                # so we are dealing with a sublib. As we build sublibs separately, the above
                # query should be safe.
                echo "--dependency=''${name#z-$P-z-}=$id" >> $out/exactDeps/$P/configure-flags
              fi
              if ver=$(${nixpkgs-ghc}/bin/ghc-pkg field $P version --simple-output); then
                echo "constraint: $P == $ver" >> $out/exactDeps/$P/cabal.config
                echo "constraint: $P installed" >> $out/exactDeps/$P/cabal.config
              fi
            done

            mkdir -p $out/evalDeps
            for P in $(${nixpkgs-ghc}/bin/ghc-pkg list --simple-output | sed 's/-[0-9][0-9.]*//g'); do
              touch $out/evalDeps/$P
              if id=$(${nixpkgs-ghc}/bin/ghc-pkg field $P id --simple-output); then
                echo "package-id $id" >> $out/evalDeps/$P
              fi
            done
          '';
    in
    pkgs.symlinkJoin {
      paths = [ nixpkgs-ghc exactDepOut ];

      name = "${nixpkgs-ghc.name}-with-evalDeps";
      inherit (nixpkgs-ghc) version;

      # BLAH
      inherit (haskell-nix-ghc) passthru;
      # enableShared = true;
      # targetPrefix = "";
      # passthru.configured-src = haskell-nix-ghc.passthru.configured-src;
      # useLLVM = false;
    };

  o2 = (
    final: prev: {
      haskell-nix = prev.haskell-nix // {
        compiler = prev.haskell-nix.compiler // {
          ghc8107 = fix final prev "ghc8107";
        };
      };
    }
  );

  pkgs = import nixpkgs {
    inherit (haskell-nix) config;
    overlays = [
      haskell-nix.overlay
    ]
    ++
    (if with-nixpkgs then [ o2 ] else [ ])
    ;
  };

  prj = pkgs.haskell-nix.cabalProject {
    src = ./empty;
    cabalProject = "extra-packages: lens";
    compiler-nix-name = "ghc8107";
  };
in

prj.plan-nix

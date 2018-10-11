{ pkg-def
, pkgs    ? import <nixpkgs> {}
, hackage ? import <hackage>
, haskell ? import <haskell> }:

{ extraDeps ? hsPkgs: {} }:
let

  # packages that we must never try to reinstall.
  nonReinstallablePkgs = [ "rts" "ghc" "ghc-prim" "integer-gmp" "integer-simple" "base"
                           "array" "deepseq" "pretty" "ghc-boot-th" "template-haskell"];

  hackagePkgs = with pkgs.lib;
                let shippedPkgs = filterAttrs (n: _: builtins.elem n nonReinstallablePkgs)
                                   (mapAttrs (name: version: { ${version} = null; })
                                     (pkg-def {}).compiler.packages);
                in recursiveUpdate hackage.exprs shippedPkgs;

  # We may depend on packages shipped with ghc, or need to rebuild them.
  ghcPackages = pkgs.lib.mapAttrs (name: version: hackagePkgs.${name}.${version}
                                  ) (pkg-def {}).compiler.packages;

  # Thus the final package set in our augmented (extrDeps) lts set is the following:
  ltsPkgs = ghcPackages
         // (pkg-def hackagePkgs).packages
         // extraDeps hackagePkgs;

  driver = haskell.compat.driver;
  host-map = haskell.compat.host-map;

  # compiler this lts set is built against.
  compiler = pkgs.haskell.packages.${(pkg-def {}).compiler.nix-name};

  # This is a tiny bit better than doJailbreak.
  #
  # We essentially *know* the dependencies, and with the
  # full cabal file representation, we also know all the
  # flags.  As such we can sidestep the solver.
  #
  # Pros:
  #  - no need for doJailbreak
  #    - no need for jailbreak-cabal to be built with
  #      Cabal2 if the cabal file requires it.
  #  - no reliance on --allow-newer, which only made
  #    a very short lived appearance in Cabal.
  #    (Cabal-2.0.0.2 -- Cabal-2.2.0.0)
  #
  # Cons:
  #  - automatic flag resolution won't happen and will
  #    have to be hard coded.
  #
  # Ideally we'd just inspect the haskell*Depends fields
  # we feed the builder. However because we null out the
  # lirbaries ghc ships (e.g. base, ghc, ...) this would
  # result in an incomplete --dependency=<name>=<name>-<version>
  # set and not lead to the desired outcome.
  #
  # If we could still have base, etc. not nulled, but
  # produce some virtual derivation, that might allow us
  # to just use the haskell*Depends fields to extract the
  # name and version for each dependency.
  #
  # Ref: https://github.com/haskell/cabal/issues/3163#issuecomment-185833150
  # ---
  # ghc-pkg should be ${ghcCommand}-pkg; and --package-db
  # should better be --${packageDbFlag}; but we don't have
  # those variables in scope.
  doExactConfig = pkgs: pkg: let targetPrefix = with pkgs.stdenv; lib.optionalString
    (hostPlatform != buildPlatform)
    "${hostPlatform.config}-";

  in pkgs.haskell.lib.overrideCabal pkg (drv: {
    # TODO: need to run `ghc-pkg field <pkg> id` over all `--dependency`
    #       values.  Should we encode the `id` in the nix-pkg as well?
    preConfigure = (drv.preConfigure or "") + ''
    configureFlags+=" --exact-configuration"
    globalPackages=$(${targetPrefix}ghc-pkg list --global --simple-output)
    localPackages=$(${targetPrefix}ghc-pkg --package-db="$packageConfDir" list --simple-output)
    for pkg in $globalPackages; do
      pkgName=''${pkg%-*}
      if [ "$pkgName" != "rts" ]; then
        if [[ " ${pkgs.lib.concatStringsSep " " nonReinstallablePkgs} " =~ " $pkgName " ]]; then
            configureFlags+=" --dependency="''${pkg%-*}=$pkg
        fi
      fi
    done
    for pkg in $localPackages; do
      configureFlags+=" --dependency="''${pkg%-*}=$pkg
    done
    #echo "<<< <<< <<<"
    #echo ''${configureFlags}
    configureFlags=$(for flag in ''${configureFlags};do case "X''${flag}" in
          X--dependency=*)
            pkgId=$(${targetPrefix}ghc-pkg --package-db="$packageConfDir" field ''${flag##*=} id || ${targetPrefix}ghc-pkg --global field ''${flag##*=} id)
            echo ''${flag%=*}=$(echo $pkgId | awk -F' ' '{ print $2 }')
            ;;
          *) echo ''${flag};;
          esac; done)
    #echo "--- --- ---"
    #echo ''${configureFlags}
    #echo ">>> >>> >>>"
'';
  });

  toGenericPackage = stackPkgs: args: name: path:
    if path == null then null else
    let expr = driver { cabalexpr = import path;
             pkgs = pkgs // { haskellPackages = stackPkgs; };
             inherit (host-map pkgs.stdenv) os arch;
             version = compiler.ghc.version; };
     # Use `callPackage` from the `compiler` here, to get the
     # right compiler.
     in compiler.callPackage expr args;

in let stackPackages = pkgs: self:
       (let p = (pkgs.lib.mapAttrs (toGenericPackage self {}) ltsPkgs);
         # for all packages do the `exactConfig` logic. That is, we
         # *know* that that our package-db contains only a single valid
         # set of proper packages. So we can sidestep cabals solver.
         in (pkgs.lib.mapAttrs (_: v: if v == null
                                      then null
                                      else doExactConfig pkgs v) p)
            // (with pkgs.haskell.lib;
            { doctest = null;
              hsc2hs = null;
              buildPackages = pkgs.buildPackages.haskellPackages; }));
   in compiler.override {
      initialPackages = { pkgs, stdenv, callPackage }: self: (stackPackages pkgs self);
      configurationCommon = { ... }: self: super: {};
      compilerConfig = self: super: {};
   }

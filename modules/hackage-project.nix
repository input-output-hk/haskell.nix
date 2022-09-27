{ lib, config, pkgs, haskellLib, ... }:
let
  inherit (config) name version revision;
in {
  _file = "haskell.nix/modules/hackage-project.nix";
  options = {
    version = lib.mkOption {
      type = lib.types.str;
      default = "latest";
      description = ''Version of the hackage package to use (defaults to "latest")'';
      apply = v: if v == "latest"
        # Lookup latest version in hackage.  Doing this in `apply` means others
        # can see the actual version in `config.version` (instead of "latest").
        then builtins.head (
          builtins.sort
            (a: b: builtins.compareVersions a b > 0)
            (builtins.attrNames pkgs.haskell-nix.hackage.${config.name}))
        else v;
    };
    revision = lib.mkOption {
      type = lib.types.str;
      default = "default";
      description = ''Hackage revision to use ("default", "r1", "r2", etc.)'';
    };
  };
  config = {
    # Avoid readDir and readFile IFD functions looking for these files in the hackage source
    # `mkOverride 1100` means this will be used in preference to the mkOption default,
    # but a `mkDefault` can still override this.
    cabalProject = lib.mkOverride 1100 ''
      packages: .
    '';
    cabalProjectLocal = lib.mkOverride 1100 null;
    cabalProjectFreeze = lib.mkOverride 1100 null;
    src = 
      let      
        tarball = config.evalPackages.fetchurl {
          url = "mirror://hackage/${name}-${version}.tar.gz";
          inherit (pkgs.haskell-nix.hackage.${name}.${version}) sha256; };
        rev = pkgs.haskell-nix.hackage.${name}.${version}.revisions.${revision};
        cabalFile = config.evalPackages.fetchurl {
          url = "https://hackage.haskell.org/package/${name}-${version}/revision/${toString rev.revNum}.cabal";
          inherit (rev) sha256;
        };
        revSuffix = lib.optionalString (rev.revNum > 0) "-r${toString rev.revNum}";
      in config.evalPackages.runCommand "${name}-${version}${revSuffix}-src" {} (''
          tmp=$(mktemp -d)
          cd $tmp
          tar xzf ${tarball}
          mv "${name}-${version}" $out
        '' + lib.optionalString (rev.revNum > 0) ''
          cp ${cabalFile} $out/${name}.cabal
        '') // {
          # TODO remove once nix >=2.4 is widely adopted (will trigger rebuilds of everything).
          # Disable filtering keeps pre ond post nix 2.4 behaviour the same.  This means that
          # the same `alex`, `happy` and `hscolour` are used to build GHC.  It also means that
          # that `tools` in the shell will be built the same.
          filterPath = { path, ... }: path;
        };
  };
}

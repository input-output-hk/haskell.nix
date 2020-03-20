# these are patched nixpkgs that include the following PRs:
# - https://github.com/NixOS/nixpkgs/pull/71216
# - https://github.com/NixOS/nixpkgs/pull/68398

let
  inherit (import ./dimension.nix) dimension;
  nixpkgsVersions = {
#   "release-18.09" = builtins.fetchTarball "https://github.com/input-output-hk/nixpkgs/archive/7e4dcacbf066a8e2d12693a9de1fb30c77081c5d.tar.gz";
    "release-19.03" = builtins.fetchTarball "https://github.com/input-output-hk/nixpkgs/archive/a8f81dc037a5977414a356dd068f2621b3c89b60.tar.gz";
    # "release-19.09" = builtins.fetchTarball "https://github.com/input-output-hk/nixpkgs/archive/3d623a406cec9052ae0a16a79ce3ce9de11236bb.tar.gz";
  };
  systems = {
    "x86_64-linux" = {};
  };
  crossSystems = {
    "x86_64-pc-mingw32" = {};
  };
  haskellNixArgs = import ./.;
in
dimension "Nixpkgs version" nixpkgsVersions (nixpkgsName: nixpkgsSrc:
  dimension "System" systems (system: _:
    # Native builds
    # TODO: can we merge this into the general case by picking an appropriate "cross system" to mean native?
    let pkgs = import nixpkgsSrc (haskellNixArgs // { inherit system; });
    in pkgs.recurseIntoAttrs {
      hello = (pkgs.haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
      iserv-proxy = pkgs.ghc-extra-packages.ghc865.iserv-proxy.components.exes.iserv-proxy;
      ghc = pkgs.recurseIntoAttrs pkgs.haskell-nix.compiler;
      tests = pkgs.lib.optionalAttrs (system == "x86_64-linux") (import ./test { inherit pkgs; });
    }
    //
    dimension "Cross system" crossSystems (crossSystem: _:
      # Cross builds
      let pkgs = import nixpkgsSrc (haskellNixArgs // { inherit system; crossSystem.config = crossSystem; });
      in pkgs.recurseIntoAttrs {
        hello = (pkgs.haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;

        iserv-proxy = pkgs.ghc-extra-packages.ghc865.iserv-proxy.components.exes.iserv-proxy;

        remote-iserv = pkgs.ghc-extra-packages.ghc865.remote-iserv.components.exes.remote-iserv;
      }
    )
  )
)

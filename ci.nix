# these are patched nixpkgs that include the following PRs:
# - https://github.com/NixOS/nixpkgs/pull/71216
# - https://github.com/NixOS/nixpkgs/pull/68398

let
  nixpkgsVersions = {
#   "release-18.09" = builtins.fetchTarball "https://github.com/input-output-hk/nixpkgs/archive/7e4dcacbf066a8e2d12693a9de1fb30c77081c5d.tar.gz";
    "release-19.03" = builtins.fetchTarball "https://github.com/input-output-hk/nixpkgs/archive/a8f81dc037a5977414a356dd068f2621b3c89b60.tar.gz";
    # "release-19.09" = builtins.fetchTarball "https://github.com/input-output-hk/nixpkgs/archive/3d623a406cec9052ae0a16a79ce3ce9de11236bb.tar.gz";
  };
  haskellNixArgs = import ./.;
  defaultNixpkgs = import (nixpkgsVersions."release-19.03") {};
  recRecurseIntoAttrs = with defaultNixpkgs; pred: x: if pred x then recurseIntoAttrs (lib.mapAttrs (n: v: if n == "buildPackages" then v else recRecurseIntoAttrs pred v) x) else x;
in
  recRecurseIntoAttrs (x: with defaultNixpkgs; lib.isAttrs x && !lib.isDerivation x) (
    builtins.mapAttrs (nixpkgsName: nixpkgsSrc: with (import nixpkgsSrc {}); {
      x86_64-linux = {
            hello = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; }));
                (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
            x86_64-pc-mingw32-hello = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; crossSystem.config = "x86_64-pc-mingw32"; }));
                (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;

            iserv-proxy = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; }));
                (ghc-extra-packages.ghc865.iserv-proxy.components.exes).iserv-proxy;

            x86_64-pc-mingw32-iserv-proxy = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; crossSystem.config = "x86_64-pc-mingw32"; }));
                (buildPackages.ghc-extra-packages.ghc865.iserv-proxy.components.exes).iserv-proxy;

            x86_64-pc-mingw32-remote-iserv = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; crossSystem.config = "x86_64-pc-mingw32"; }));
                (ghc-extra-packages.ghc865.remote-iserv.components.exes).remote-iserv;

      };
      x86_64-darwin = {
            hello = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; }));
                (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
            x86_64-pc-mingw32-hello = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; crossSystem.config = "x86_64-pc-mingw32"; }));
                (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;

            iserv-proxy = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; }));
                (ghc-extra-packages.ghc865.iserv-proxy.components.exes).iserv-proxy;

            x86_64-pc-mingw32-iserv-proxy = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; crossSystem.config = "x86_64-pc-mingw32"; }));
                (buildPackages.ghc-extra-packages.ghc865.iserv-proxy.components.exes).iserv-proxy;

            x86_64-pc-mingw32-remote-iserv = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; crossSystem.config = "x86_64-pc-mingw32"; }));
                (ghc-extra-packages.ghc865.remote-iserv.components.exes).remote-iserv;

      };
      haskell-nix.compiler = {
        x86_64-linux = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; }));
            haskell-nix.compiler;
        x86_64-darwin = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin";}));
            haskell-nix.compiler;
      };
      tests = {
        x86_64-linux = (import ./test { nixpkgs = nixpkgsSrc; nixpkgsArgs = { system = "x86_64-linux"; }; });
        # x86_64-darwin = (import ./test { nixpkgs = nixpkgsSrc; nixpkgsArgs = { system = "x86_64-darwin"; }; });
      };
      examples = let
        cardano-sl-args = import ./examples/cardano-sl-args.nix;
        cardano-wallet-args = import ./examples/cardano-wallet-args.nix;
        plutus-args = import ./examples/plutus-args.nix;
      in {
        x86_64-linux = {
                # cardano-sl
                cardano-sl = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; }));
                    (lib.filterAttrs (k: _: builtins.elem k cardano-sl-args.pkgs)
                        (haskell-nix.stackProject cardano-sl-args));
                x86_64-pc-mingw32-cardano-sl = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; crossSystem.config = "x86_64-pc-mingw32"; }));
                    (lib.filterAttrs (k: _: builtins.elem k cardano-sl-args.pkgs)
                        (haskell-nix.stackProject cardano-sl-args));

                # cardano-wallet
                cardano-wallet = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; }));
                    (lib.filterAttrs (k: _: builtins.elem k cardano-wallet-args.pkgs)
                        (haskell-nix.stackProject cardano-wallet-args));
                x86_64-pc-mingw32-cardano-wallet = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; crossSystem.config = "x86_64-pc-mingw32"; }));
                    (lib.filterAttrs (k: _: builtins.elem k cardano-wallet-args.pkgs)
                        (haskell-nix.stackProject cardano-wallet-args));
                plutus = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; }));
                    (lib.filterAttrs (k: _: builtins.elem k plutus-args.pkgs)
                        (haskell-nix.stackProject plutus-args));
                x86_64-pc-mingw32-plutus = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; crossSystem.config = "x86_64-pc-mingw32"; }));
                    (lib.filterAttrs (k: _: builtins.elem k plutus-args.pkgs)
                        (haskell-nix.stackProject plutus-args));

        };
        x86_64-darwin = {
                # cardano-sl
                cardano-sl = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; }));
                    (lib.filterAttrs (k: _: builtins.elem k cardano-sl-args.pkgs)
                        (haskell-nix.stackProject cardano-sl-args));
                x86_64-pc-mingw32-cardano-sl = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; crossSystem.config = "x86_64-pc-mingw32"; }));
                    (lib.filterAttrs (k: _: builtins.elem k cardano-sl-args.pkgs)
                        (haskell-nix.stackProject cardano-sl-args));

                # cardano-wallet
                cardano-wallet = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; }));
                    (lib.filterAttrs (k: _: builtins.elem k cardano-wallet-args.pkgs)
                        (haskell-nix.stackProject cardano-wallet-args));
                x86_64-pc-mingw32-cardano-wallet = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; crossSystem.config = "x86_64-pc-mingw32"; }));
                    (lib.filterAttrs (k: _: builtins.elem k cardano-wallet-args.pkgs)
                        (haskell-nix.stackProject cardano-wallet-args));
                plutus = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; }));
                    (lib.filterAttrs (k: _: builtins.elem k plutus-args.pkgs)
                        (haskell-nix.stackProject plutus-args));
                x86_64-pc-mingw32-plutus = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; crossSystem.config = "x86_64-pc-mingw32"; }));
                    (lib.filterAttrs (k: _: builtins.elem k plutus-args.pkgs)
                        (haskell-nix.stackProject plutus-args));
        };
    };

# Don't build (all of) stackage on linux for now.
#    stackage = {
#        x86_64-linux = (with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; }));
#            haskell-nix.snapshots."lts-13.29");
#        # x86_64-darwin = (with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; }));
#        #     haskell-nix.snapshots."lts-13.29");
#    };

}) nixpkgsVersions)

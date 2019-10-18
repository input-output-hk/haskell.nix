# these are patched nixpkgs that include the following PRs:
# - https://github.com/NixOS/nixpkgs/pull/71216
# - https://github.com/NixOS/nixpkgs/pull/68398
let nixpkgs1903 = builtins.fetchTarball "https://github.com/input-output-hk/nixpkgs/archive/a8f81dc037a5977414a356dd068f2621b3c89b60.tar.gz";
in with (import nixpkgs1903 {});
let
    haskellNixArgs = import ./.;
    recRecurseIntoAttrs = pred: x: if pred x then recurseIntoAttrs (lib.mapAttrs (n: v: if n == "buildPackages" then v else recRecurseIntoAttrs pred v) x) else x;
in recRecurseIntoAttrs (x: lib.isAttrs x && !lib.isDerivation x) {
    "release-19.03" = {
       x86_64-linux = {
            hello = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-linux"; }));
                (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
            x86_64-pc-mingw32-hello = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-linux"; crossSystem.config = "x86_64-pc-mingw32"; }));
                (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;

            iserv-proxy = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-linux"; }));
                (ghc-extra-packages.ghc865.iserv-proxy.components.exes).iserv-proxy;

            x86_64-pc-mingw32-iserv-proxy = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-linux"; crossSystem.config = "x86_64-pc-mingw32"; }));
                (buildPackages.ghc-extra-packages.ghc865.iserv-proxy.components.exes).iserv-proxy;

            x86_64-pc-mingw32-remote-iserv = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-linux"; crossSystem.config = "x86_64-pc-mingw32"; }));
                (ghc-extra-packages.ghc865.remote-iserv.components.exes).remote-iserv;

        };
        x86_64-darwin = {
            hello = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-darwin"; }));
                (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
            x86_64-pc-mingw32-hello = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-darwin"; crossSystem.config = "x86_64-pc-mingw32"; }));
                (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;

            iserv-proxy = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-darwin"; }));
                (ghc-extra-packages.ghc865.iserv-proxy.components.exes).iserv-proxy;

            x86_64-pc-mingw32-iserv-proxy = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-darwin"; crossSystem.config = "x86_64-pc-mingw32"; }));
                (buildPackages.ghc-extra-packages.ghc865.iserv-proxy.components.exes).iserv-proxy;

            x86_64-pc-mingw32-remote-iserv = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-darwin"; crossSystem.config = "x86_64-pc-mingw32"; }));
                (ghc-extra-packages.ghc865.remote-iserv.components.exes).remote-iserv;

        };
    };
    haskell.compiler = {
        x86_64-linux = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-linux"; }));
            haskell.compiler;
        x86_64-darwin = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-darwin";}));
            haskell.compiler;
    };
    tests = {
        x86_64-linux = (import ./test { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-linux"; }; });
        # x86_64-darwin = (import ./test { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-darwin"; }; });
    };
    examples = let
        iohk-archive = name: hash: "https://github.com/input-output-hk/${name}/archive/${hash}.tar.gz";
        cardano-sl = rec {
            src = builtins.fetchTarball (iohk-archive "cardano-sl" "1a792d7cd0f0c93a0f0c28f66372bce3c3808dbd");
        };
        cardano-wallet-args = rec {
            src = builtins.fetchTarball (iohk-archive "cardano-wallet" "d525e85fe19a37d8b5648ac783ef35474be38bcc");
            # wee need these, as they are referenced in the stack.yaml file; however we can't
            # fetch them at IFD time as they don't provide the sha256. Hence we use the cache-file
            # facility on stack-to-nix and pre-populate the cache file with the relevant hashes.
            cache = [
                { name = "cardano-crypto";
                    url = "https://github.com/input-output-hk/cardano-crypto";
                    rev = "3c5db489c71a4d70ee43f5f9b979fcde3c797f2a";
                    sha256 = "0lss4x41m0ylhximqjc56ps0y3pag3x58wm480pzfa48lpk4gqpk"; }
                { name = "persistent-sqlite";
                    url = "https://github.com/KtorZ/persistent";
                    rev = "79f2ece07eafae005a703c8eda1bd2420b5e07b5";
                    sha256 = "081bhdg52wn7vgxsgl4aimy73ccai05j64r24hwkdnjj4kz96lia";
                    subdir = "persistent-sqlite"; }
                { name = "iohk-monitoring";
                    url = "https://github.com/input-output-hk/iohk-monitoring-framework";
                    rev = "bd31cd2f3922010ddb76bb869f29c4e63bb8001b";
                    subdir = "iohk-monitoring";
                    sha256 = "1dfk505qbpk6p3gcpxa31wmg98qvx9hlrxlf0khaj7hizf3b8b60"; }
                { name = "contra-tracer";
                    url = "https://github.com/input-output-hk/iohk-monitoring-framework";
                    rev = "bd31cd2f3922010ddb76bb869f29c4e63bb8001b";
                    subdir = "contra-tracer";
                    sha256 = "1dfk505qbpk6p3gcpxa31wmg98qvx9hlrxlf0khaj7hizf3b8b60"; }
            ];
            # This is needed due to some deficiencies in stackage snapshots.
            # maybe we should make this part of some `stack-default-pkg-def-extras`?
            # We have similar logic in the snapshots.nix file to deal with this in
            # snapshots.
            #
            # TODO: move this somewhere into stackProject
            pkg-def-extras = [
                (hackage: {
                    packages = {
                        "transformers" = (((hackage.transformers)."0.5.6.2").revisions).default;
                        "process" = (((hackage.process)."1.6.5.0").revisions).default;
                    };
                })
                (hackage: {
                    packages = {
                        "hsc2hs" = (((hackage.hsc2hs)."0.68.4").revisions).default;
                    };
                })
            ];
            modules = [
                # for each item in the `cache`, set
                #   packages.$name.src = fetchgit ...
                # and
                #   packages.$name.postUnpack = ...
                # if subdir is given.
                #
                # We need to do this, as cabal-to-nix will generate
                # src = /nix/store/... paths, and when we build the
                # package we won't have access to the /nix/store
                # path.  As such we regenerate the fetchgit command
                # we used in the first place, and thus have a proper
                # src value.
                #
                # TODO: this should be moved into `call-stack-to-nix`
                #       it should be automatic and not the burden of
                #       the end user to work around nix peculiarities.
                { packages = builtins.foldl' (x: y: x // y) {}
                    (builtins.map ({ name, url, rev, sha256, subdir ? null }:
                        { ${name} = { src = fetchgit { inherit url rev sha256; }; }
                                // lib.optionalAttrs (subdir != null) { postUnpack = "sourceRoot+=/${subdir}; echo source root reset to $sourceRoot"; };
                        }) cache);
                }
            ];
        };
    in {
        "release-19.03" = {
            x86_64-linux = {
                cardano-sl = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-linux"; }));
                    (haskell-nix.stackProject cardano-sl);
                cardano-wallet = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-linux"; }));
                    (haskell-nix.stackProject cardano-wallet-args);#.cardano-wallet-jormungandr.components.all;
                x86_64-pc-mingw32-cardano-sl = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-linux"; crossSystem.config = "x86_64-pc-mingw32"; }));
                    (haskell-nix.stackProject cardano-sl);
                x86_64-pc-mingw32-cardano-wallet = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-linux"; crossSystem.config = "x86_64-pc-mingw32"; }));
                    (haskell-nix.stackProject cardano-wallet-args);#.cardano-wallet-jormungandr.components.all;
            };
            x86_64-darwin = {
                cardano-wallet = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-darwin"; }));
                    (haskell-nix.stackProject cardano-wallet-args);#.cardano-wallet-jormungandr.components.all;
                x86_64-pc-mingw32-cardano-wallet = with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-darwin"; crossSystem.config = "x86_64-pc-mingw32"; }));
                    (haskell-nix.stackProject cardano-wallet-args);#.cardano-wallet-jormungandr.components.all;
            };
        };
    };
    # Needs agent redeploy
    #

# Don't build (all of) stackage on linux for now.
#    stackage = {
#        x86_64-linux = (with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-linux"; }));
#            haskell-nix.snapshots."lts-13.29");
#        # x86_64-darwin = (with (import nixpkgs1903 (haskellNixArgs // { system = "x86_64-darwin"; }));
#        #     haskell-nix.snapshots."lts-13.29");
#    };

}

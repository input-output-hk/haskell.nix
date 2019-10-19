# these are patched nixpkgs that include the following PRs:
# - https://github.com/NixOS/nixpkgs/pull/71216
# - https://github.com/NixOS/nixpkgs/pull/68398

let
  nixpkgsVersions = {
    "release-18.09" = builtins.fetchTarball "https://github.com/input-output-hk/nixpkgs/archive/7e4dcacbf066a8e2d12693a9de1fb30c77081c5d.tar.gz";
    "release-19.03" = builtins.fetchTarball "https://github.com/input-output-hk/nixpkgs/archive/a8f81dc037a5977414a356dd068f2621b3c89b60.tar.gz";
    "release-19.09" = builtins.fetchTarball "https://github.com/input-output-hk/nixpkgs/archive/3d623a406cec9052ae0a16a79ce3ce9de11236bb.tar.gz";
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
      haskell.compiler = {
        x86_64-linux = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; }));
            haskell.compiler;
        x86_64-darwin = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin";}));
            haskell.compiler;
      };
      tests = {
        x86_64-linux = (import ./test { nixpkgs = nixpkgsSrc; nixpkgsArgs = { system = "x86_64-linux"; }; });
        # x86_64-darwin = (import ./test { nixpkgs = nixpkgsSrc; nixpkgsArgs = { system = "x86_64-darwin"; }; });
      };
      examples = let
        iohk-archive = name: hash: "https://github.com/input-output-hk/${name}/archive/${hash}.tar.gz";
        mkCacheModule = cache:
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
            };
        cardano-sl-args = rec {
            src = builtins.fetchTarball (iohk-archive "cardano-sl" "1a792d7cd0f0c93a0f0c28f66372bce3c3808dbd");
            pkgs = [
                "cardano-sl-crypto-test"
                "cardano-sl-crypto"
                "cardano-sl-cluster"
                "cardano-sl-infra"
                "cardano-sl-infra-test"
                "cardano-sl-tools"
                "cardano-sl-tools-post-mortem"
                "cardano-sl-core"
                "cardano-sl-core-test"
                "cardano-sl-util-test"
                "cardano-sl-util"
                "cardano-sl-script-runner"
                "cardano-sl-auxx"
                "cardano-sl-networking"
                "cardano-sl-explorer"
                "cardano-sl-chain-test"
                "cardano-sl-chain"
                "cardano-sl-mnemonic"
                "cardano-sl-generator"
                "cardano-sl-x509"
                "cardano-sl-utxo"
                "cardano-sl-node-ipc"
                "cardano-wallet"
                "cardano-sl"
                "cardano-sl-db-test"
                "cardano-sl-db"
                "cardano-sl-faucet"
                "cardano-sl-binary-test"
                "cardano-sl-binary"
                "cardano-sl-node"
                "cardano-sl-client"
            ];
            cache = [
                { name = "cardano-crypto";
                    url = "https://github.com/input-output-hk/cardano-crypto";
                    rev = "4590efa638397e952a51a8994b5543e4ea3c1ecd";
                    sha256 = "0hl2n3bba5v2j0lmxhs7hs01z3aznh2bwf9cb434icq2g0bl8ms3"; }
                { name = "haskell-ip";
                    cabal-file = "ip.cabal";
                    url = "https://github.com/andrewthad/haskell-ip";
                    rev = "9bb453139aa82cc973125091800422a523e1eb8f";
                    sha256 = "199mfpbgca7rvwvwk2zsmcpibc0sk0ni7c5zlf4gk3cps8s85gyr"; }
                { name = "time-units";
                    url = "https://github.com/serokell/time-units.git";
                    rev = "6c3747c1ac794f952de996dd7ba8a2f6d63bf132";
                    sha256 = "0psdr1if0rgnn24698x3583m0603rwd3sd7yb9whj03hskmkwpgs";
                }
                { name = "kademlia";
                    url = "https://github.com/serokell/kademlia.git";
                    rev = "7120bb4d28e708acd52dfd61d3dca7914fac7d7f";
                    sha256 = "1k1wp9dwhzzqfivxc28vhxfqplnyh916crr7bhsiv829d6qifhw1";
                }
                { name = "network-transport";
                    url = "https://github.com/serokell/network-transport";
                    rev = "018a50b9042c2115c3ec9c9fd5ca5f28737dd29c";
                    sha256 = "0lqa26l2ikpq6a4s7qm9b2favx59w82i0wngakhfyax66fpixp8q";
                }
                { name = "network-transport-tcp";
                    url = "https://github.com/avieth/network-transport-tcp";
                    rev = "2634e5e32178bb0456d800d133f8664321daa2ef";
                    sha256 = "03shp9prflhgqzw7pymw1pq2q7s1wf46pyjm2csx8646snrhf35i";
                }
                { name = "network-transport-inmemory";
                    url = "https://github.com/avieth/network-transport-inmemory";
                    rev = "5d8ff2b07b9df35cf61329a3d975e2c8cf95c12a";
                    sha256 = "0ak64rks3lk3kk5wyndrrk2swmd84h9diribzix305xwz1jhjj9w";
                }
                { name = "acid-state";
                    url = "https://github.com/parsonsmatt/acid-state";
                    rev = "a1b23e2056f134e53f705a694ab85deeecabec5c";
                    sha256 = "0mgdk8252g7wbb0afyn21pcn3bwh4vainy3h2d0xsv4hlpgqgnw8";
                }
                { name = "socket-io";
                    url = "https://github.com/input-output-hk/engine.io";
                    rev = "d3c55f51bb81cee7d0d551de930ce65fe7d76756";
                    sha256 = "139c0yfnj57cpwg4k0am2rp35sh959394nvlb98011rjy68200qc";
                    subdir = "socket-io";
                }
                { name = "engine-io";
                    url = "https://github.com/input-output-hk/engine.io";
                    rev = "d3c55f51bb81cee7d0d551de930ce65fe7d76756";
                    sha256 = "139c0yfnj57cpwg4k0am2rp35sh959394nvlb98011rjy68200qc";
                    subdir = "engine-io";
                }
                { name = "canonical-json";
                    url = "https://github.com/well-typed/canonical-json.git";
                    rev = "ddfe3593b80b5ceb88842bb7a6f2268df75d2c2f";
                    sha256 = "02fzn1xskis1lc1pkz0j92v6ipd89ww0k2p3dvwpm3yap5dpnm7k";
                }
                { name = "clock";
                    url = "https://github.com/corsis/clock.git";
                    rev = "ef60bd51a3587a173adf565c33bf2886df6e3842";
                    sha256 = "1r4n9imls483f7wd61fi1jk16z2k7w36gpx798sqidvwbnc831q1";
                }
                { name = "rocksdb-haskell-ng";
                    url = "https://github.com/input-output-hk/rocksdb-haskell-ng.git";
                    rev = "49f501a082d745f3b880677220a29cafaa181452";
                    sha256 = "02jvri8ik8jgrxwa6qmh3xcwqvm4s27iv3sxpjpny79nlhlxvfzp";
                }
                { name = "log-warper";
                    url = "https://github.com/input-output-hk/log-warper";
                    rev = "5271ab6c33541b8155ca203e714875974ec116be";
                    sha256 = "1h14004a8iwr8nw31pqq6kfdhfsyzzl8a50hrmj2acjqq6mbdl2m";
                }
                { name = "universum";
                    url = "https://github.com/input-output-hk/universum";
                    rev = "15f7db758ff5a24b874de2247f6f7a4576562da5";
                    sha256 = "127bs29zpjcc40777dv7figk05gd00b9ja57sp11w440qr5h72hk";
                }
                { name = "serokell-util";
                    url = "https://github.com/input-output-hk/serokell-util";
                    rev = "457f1d149c6e238841f283a1faf7bc8fb021b27d";
                    sha256 = "0llbryqna9p03db3ka4933vzf4jw1yxdbsf7cqi2pivsy1vp6kp0";
                }
                { name = "inspector";
                    url = "https://github.com/primetype/inspector.git";
                    rev = "c975f4329365f0379c04358138e616fb96fb0b79";
                    sha256 = "12q1v7a8kcw7qi4lws4j3mvxwfkhni6zmp870kmnkgbgwvrax9gs";
                }
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
            modules = [ (mkCacheModule cache) ];
        };
        cardano-wallet-args = rec {
            src = builtins.fetchTarball (iohk-archive "cardano-wallet" "d525e85fe19a37d8b5648ac783ef35474be38bcc");
            pkgs = [
                "bech32"
                "text-class"
                "cardano-wallet-http-bridge"
                "cardano-wallet-launcher"
                "cardano-wallet-test-utils"
                "cardano-wallet-core"
                "cardano-wallet-jormungandr"
                "cardano-wallet-cli"
                "cardano-crypto"
                "cardano-wallet-core-integration"
            ];
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
            modules = [ (mkCacheModule cache) ];
        };
        plutus-args = rec {
            src = builtins.fetchTarball (iohk-archive "plutus" "0d76e98ab267d621ad15002b5f824cad71797754");
            cache = [
                { name = "row-types";
                    url = "https://github.com/target/row-types";
                    rev = "1e8d5e084ffd46f6c7842826a1f62c60820885df";
                    sha256 = "0ly5m4r8wkm8gdqyrqzsjfmp189yxsd4qp0zi3idrbgfaf45sk9k"; }
                { name = "purescript-bridge";
                    url = "https://github.com/shmish111/purescript-bridge.git";
                    rev = "0042602f8a195b1fe185138f9ccca02020b8dd62";
                    sha256 = "1vl88g41f4vvgw9iyn7zd7i52qshpnk02z0y6czg4zy7wk3q12gz"; }
                { name = "servant-purescript";
                    url = "https://github.com/shmish111/servant-purescript.git";
                    rev = "ece5d1dad16a5731ac22040075615803796c7c21";
                    sha256 = "1axcbsaym64q67hvjc7b3izd48cgqwi734l7f7m22jpdc80li5f6"; }
                { name = "cardano-crypto";
                    url = "https://github.com/input-output-hk/cardano-crypto.git";
                    rev = "f5cecb6e424cc84f85b6a3e1f803517bb7b4cfb1";
                    sha256 = "1jyzai4sn9hi3p6r97h54f9a7an7vk38lwrbl4mds9zmdsw5f5ad"; }
                { name = "unlit";
                    url = "https://github.com/michaelpj/unlit.git";
                    rev = "9ca1112093c5ffd356fc99c7dafa080e686dd748";
                    sha256 = "145sffn8gbdn6xp9q5b75yd3m46ql5bnc02arzmpfs6wgjslfhff"; }
                { name = "prometheus";
                    url = "https://github.com/bitnomial/prometheus.git";
                    rev = "69e4cefeb7d04d61a54cb0ae9fd57e2de134badb";
                    sha256 = "0h836qp0ic587gfyfkj9a53p8rczja50sfy2y8ymx8wwkmq9zdgc"; }
                { name = "github";
                    url = "https://github.com/shmish111/github.git";
                    rev = "cc27b9de4d5d0939235fa9a8b418de3ea4807bab";
                    sha256 = "0jx65x4c8s561nbxa0hv7mp8fvndfhs8i8gzxng7ym808n4n538i"; }
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
            modules = [ (mkCacheModule cache) ];
        };
      in {
        x86_64-linux = {
                # cardano-sl
                cardano-sl = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; }));
                    (lib.filterAttrs (k: _: builtins.elem k cardano-wallet-sl.pkgs)
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
                    (haskell-nix.stackProject plutus-args).language-plutus-core.components.all;
                x86_64-pc-mingw32-plutus = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; crossSystem.config = "x86_64-pc-mingw32"; }));
                    (haskell-nix.stackProject plutus-args).language-plutus-core.components.all;
        };
        x86_64-darwin = {
                # cardano-sl
                cardano-sl = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; }));
                    (lib.filterAttrs (k: _: builtins.elem k cardano-wallet-sl.pkgs)
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
                    (haskell-nix.stackProject plutus-args).language-plutus-core.components.all;
                x86_64-pc-mingw32-plutus = with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; crossSystem.config = "x86_64-pc-mingw32"; }));
                    (haskell-nix.stackProject plutus-args).language-plutus-core.components.all;
        };
    };
    # Needs agent redeploy
    #

# Don't build (all of) stackage on linux for now.
#    stackage = {
#        x86_64-linux = (with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-linux"; }));
#            haskell-nix.snapshots."lts-13.29");
#        # x86_64-darwin = (with (import nixpkgsSrc (haskellNixArgs // { system = "x86_64-darwin"; }));
#        #     haskell-nix.snapshots."lts-13.29");
#    };

}) nixpkgsVersions)

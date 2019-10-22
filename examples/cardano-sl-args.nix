let iohk-archive = name: hash: "https://github.com/input-output-hk/${name}/archive/${hash}.tar.gz";
in rec {
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
        { name = "engine-io-wai";
            url = "https://github.com/input-output-hk/engine.io";
            rev = "d3c55f51bb81cee7d0d551de930ce65fe7d76756";
            sha256 = "139c0yfnj57cpwg4k0am2rp35sh959394nvlb98011rjy68200qc";
            subdir = "engine-io-wai";
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
}

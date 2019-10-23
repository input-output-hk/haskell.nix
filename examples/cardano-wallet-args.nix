let iohk-archive = name: hash: "https://github.com/input-output-hk/${name}/archive/${hash}.tar.gz";
in rec {
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
}

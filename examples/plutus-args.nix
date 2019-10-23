let iohk-archive = name: hash: "https://github.com/input-output-hk/${name}/archive/${hash}.tar.gz";
in rec {
    src = builtins.fetchTarball (iohk-archive "plutus" "0d76e98ab267d621ad15002b5f824cad71797754");
    pkgs = [
        "language-plutus-core"
        "plutus-core-interpreter"
        "plutus-exe"
        "plutus-ir"
        "plutus-tx"
        "plutus-use-cases"
        "playground-common"
        "marlowe"
        "marlowe-playground-server"
        "plutus-wallet-api"
        "plutus-playground-server"
        "plutus-playground-lib"
        "plutus-tutorial"
        "plutus-book"
        "plutus-contract"
        "plutus-emulator"
        "deployment-server"
        "iots-export"
        "marlowe-symbolic"
    ];
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
}

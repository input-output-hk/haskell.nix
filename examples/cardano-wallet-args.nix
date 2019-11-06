let iohk-archive = name: hash: "https://github.com/input-output-hk/${name}/archive/${hash}.tar.gz";
    src = builtins.fetchTarball (iohk-archive "cardano-wallet" "1fbd206bf6b368f08d603db130648ebd4efcedbc");
    iohkLib = import (src + "/nix/iohk-common.nix") {};
    jmPkgs = import (src + "/nix/jormungandr.nix") { inherit iohkLib; };

in rec {
    inherit src;
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
        { name = "persistent";
            url = "https://github.com/input-output-hk/persistent";
            rev = "107787ecc4c8a112375493cd66574f788f950fce";
            sha256 = "1livmfslqzadir5sc523r111wrd14s5k2nmvsxayk45hx3nnngfb";
            subdir = "persistent"; }
        { name = "persistent-sqlite";
            url = "https://github.com/input-output-hk/persistent";
            rev = "107787ecc4c8a112375493cd66574f788f950fce";
            sha256 = "1livmfslqzadir5sc523r111wrd14s5k2nmvsxayk45hx3nnngfb";
            subdir = "persistent-sqlite"; }
        { name = "persistent-template";
            url = "https://github.com/input-output-hk/persistent";
            rev = "107787ecc4c8a112375493cd66574f788f950fce";
            sha256 = "1livmfslqzadir5sc523r111wrd14s5k2nmvsxayk45hx3nnngfb";
            subdir = "persistent-template"; }
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
                "hsc2hs" = (((hackage.hsc2hs)."0.68.4").revisions).default;
            };
        })
    ];

    modules = [
      # Add dependencies
      {
        packages.cardano-wallet-jormungandr.components.tests = {
          # Some tests want to write ~/.local/share/cardano-wallet
          integration.preBuild = "export HOME=`pwd`";
          # provide jormungandr command to test suites
          integration.build-tools = [
            jmPkgs.jormungandr
            jmPkgs.jormungandr-cli
          ];
          unit.build-tools = [ jmPkgs.jormungandr ];
        };

        packages.cardano-wallet-core.components.tests.unit.preBuild = ''
          export SWAGGER_YAML=${src + "/specifications/api/swagger.yaml"}
        '';
      }

      # Misc. build fixes for dependencies
      {
        # Cut down iohk-monitoring deps
        packages.iohk-monitoring.flags = {
          disable-ekg = true;
          disable-examples = true;
          disable-graylog = true;
          disable-gui = true;
          disable-prometheus = true;
          disable-systemd = true;
        };

        # Katip has Win32 (>=2.3 && <2.6) constraint
        packages.katip.doExactConfig = true;
      }
    ];
}

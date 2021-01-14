final: prev:
{
   haskell-nix = prev.haskell-nix // ({
     defaultModules = prev.haskell-nix.defaultModules ++ [
      ({ pkgs, buildModules, config, lib, ... }:
      let
        withTH = import ./linux-cross.nix {
          inherit (pkgs.stdenv) hostPlatform buildPlatform;
          inherit (pkgs) stdenv lib writeScriptBin;
          # qemu for linux
          qemu = pkgs.buildPackages.qemu;

#          wine = pkgs.buildPackages.winePackages.minimal;
#          inherit (pkgs.windows) mingw_w64_pthreads;
          inherit (pkgs) gmp;
          # iserv-proxy needs to come from the buildPackages, as it needs to run on the
          # build host.
          inherit (final.buildPackages.ghc-extra-packages."${config.compiler.nix-name}".iserv-proxy.components.exes) iserv-proxy;
          # remote-iserv however needs to come from the regular packages as it has to
          # run on the target host.
          inherit (final.ghc-extra-packages."${config.compiler.nix-name}".remote-iserv.components.exes) remote-iserv;
          # we need to use openssl.bin here, because the .dll's are in the .bin expression.
          extra-test-libs = [
            # pkgs.rocksdb
            pkgs.openssl.bin
            pkgs.libffi
            pkgs.gmp
          ];
        } // {
          # we can perform testing of cross compiled test-suites by using wine.
          # Therefore let's enable doCrossCheck here!
          doCrossCheck = pkgs.stdenv.hostPlatform.isWindows;
        };
      in {
        packages = {
          # clock 0.7.2 needs to be patched to support cross compilation.
          clock.patches              = pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isAarch32 [ ({ version, revision }: (if version == "0.7.2" then ./patches/clock-0.7.2.patch else null)) ];
          # nix calls this package crypto
        #   cryptonite-openssl.patches = pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version, revision }: if version == "0.7" then ./patches/cryptonite-openssl-0.7.patch else null) ];

        #   http-client.patches        = pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version, revision }: if version == "0.5.14" then ./patches/http-client-0.5.14.patch else null) ];

        #   conduit.patches            = pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version, revision }: if builtins.compareVersions version "1.3.1.1" < 0 then ./patches/conduit-1.3.0.2.patch else null) ];
        #   streaming-commons.patches  = pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ./patches/streaming-commons-0.2.0.0.patch ];
        #   x509-system.patches        = pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ./patches/x509-system-1.6.6.patch ];
        #   file-embed-lzma.patches    = pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ./patches/file-embed-lzma-0.patch ];

          # Set all of these to [], as these form the
          # dependency graph of the libiserv, iserv-proxy, and iserv-remote
          # packages.  Subsequently we do not want the defaults that `withTH`
          # `-fexternal-interpreter` would install here.  That would ultimately
          # result in cyclic dependencies as it injects `remote-iserv` and
          # `iserv-proxy` as a dependency into every package.
          bytestring.setupBuildFlags = [];
          containers.setupBuildFlags = [];
          binary.setupBuildFlags = [];
          filepath.setupBuildFlags = [];
          time.setupBuildFlags = [];
          Win32.setupBuildFlags = [];
          libiserv.setupBuildFlags = [];
          remote-iserv.setupBuildFlags = [];
          directory.setupBuildFlags = [];
          ghc-boot.setupBuildFlags = [];
          transformers.setupBuildFlags = [];
          ghci.setupBuildFlags = [];
          network.setupBuildFlags = [];
          unix.setupBuildFlags = [];
        };
      }// withTH
      )
     ];
   });
}

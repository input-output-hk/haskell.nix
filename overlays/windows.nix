# Note: We *can not* use a global `lib.optionalAttrs hostPlatform.isWindows`, the
# conditionals need to be in the leafs! If we attach the conditional to the root
# node (e.g. the whole customization here), they will be evaluated at the wrong time
# and not end up with the expected changes we want.
self: super:
{
   # on windows we have this habit of putting libraries
   # into `bin`, wheras on unix it's usually `lib`. For
   # this confuses nix easily. So we'll just move the
   # .dll's from `bin` into `$out/lib`. Such that they
   # are trivially found.
  #  openssl = super.openssl.overrideAttrs (drv: {
  #   #  postInstall = with super.stdenv; drv.postInstall + lib.optionalString hostPlatform.isWindows ''
  #   #    cp $bin/bin/*.dll $out/lib/
  #   #  '';
  #   postFixup = "";
  #  });

   mfpr = super.mfpr.overrideAttrs (drv: {
     configureFlags = (drv.configureFlags or []) ++ super.stdenv.lib.optional super.stdenv.hostPlatform.isWindows "--enable-static --disable-shared" ;
   });

   libmpc = super.libmpc.overrideAttrs (drv: {
     configureFlags = (drv.configureFlags or []) ++ super.stdenv.lib.optional super.stdenv.hostPlatform.isWindows "--enable-static --disable-shared" ;
   });

   haskell-nix = super.haskell-nix // ({
     defaultModules = super.haskell-nix.defaultModules ++ [
      ({ pkgs, buildModules, config, lib, ... }:
      let
        withTH = import ./mingw_w64.nix {
          inherit (pkgs.stdenv) hostPlatform;
          inherit (pkgs) stdenv lib writeScriptBin;
          wine = pkgs.buildPackages.winePackages.minimal;
          inherit (pkgs.windows) mingw_w64_pthreads;
          inherit (pkgs) gmp;
          # iserv-proxy needs to come from the buildPackages, as it needs to run on the
          # build host.
          inherit (config.hsPkgs.buildPackages.iserv-proxy.components.exes) iserv-proxy;
          # remote-iserv however needs to come from the regular packages as it has to
          # run on the target host.
          inherit (config.hsPkgs.remote-iserv.components.exes) remote-iserv;
          # we need to use openssl.bin here, because the .dll's are in the .bin expression.
          extra-test-libs = [ pkgs.rocksdb pkgs.openssl.bin pkgs.libffi ];
        } // {
          # we can perform testing of cross compiled test-suites by using wine.
          # Therfore let's enable doCrossCheck here!
          doCrossCheck = pkgs.stdenv.hostPlatform.isWindows;
        };
      in {
        packages = {
          # This needs true, otherwise we miss most of the interesting
          # modules.
          ghci.flags.ghci = true;
          # I hope we can apply this globally.
          ghc.flags.ghci = true;

          # this needs to be true to expose module
          #  Message.Remote
          # as needed by libiserv.
          libiserv.flags.network = true;

          # libiserv has a bit too restrictive boundaries.
          # as such it won't build with newer network libraries.
          # to avoid that we use doExactConfig, which forces cabal
          # to forgoe its solver and just take the libraries it's
          # provided with.
          ghci.components.library.doExactConfig = true;
          libiserv.components.library.doExactConfig = true;
          # same for iserv-proxy
          iserv-proxy.components.exes.iserv-proxy.doExactConfig = true;
          remote-iserv.components.exes.remote-iserv.doExactConfig = true;
          remote-iserv.postInstall = pkgs.stdenv.lib.optionalString pkgs.stdenv.hostPlatform.isWindows ''
            cp ${pkgs.libffi}/bin/*.dll $out/bin/
          '';

          # Apply https://github.com/haskell/cabal/pull/6055
          # See also https://github.com/input-output-hk/iohk-nix/issues/136
          # Cabal.patches = [ ({ version, revision }: (if builtins.compareVersions version "3.0.0" < 0
          #   then pkgs.fetchpatch {
          #     url = "https://patch-diff.githubusercontent.com/raw/haskell/cabal/pull/6055.diff";
          #     sha256 = "145g7s3z9q8d18pxgyngvixgsm6gmwh1rgkzkhacy4krqiq0qyvx";
          #     stripLen = 1;
          #   }
          #   else null)) ];

          # clock 0.7.2 needs to be patche to support cross compilation.
          clock.patches              = pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version, revision }: (if version == "0.7.2" then ./patches/clock-0.7.2.patch else null)) ];
          # nix calles this package crypto
          cryptonite-openssl.patches = pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version, revision }: if version == "0.7" then ./patches/cryptonite-openssl-0.7.patch else null) ];

          conduit.patches            = pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version, revision }: if builtins.compareVersions version "1.3.1.1" < 0 then ./patches/conduit-1.3.0.2.patch else null) ];
          streaming-commons.patches  = pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ./patches/streaming-commons-0.2.0.0.patch ];
          x509-system.patches        = pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ./patches/x509-system-1.6.6.patch ];
          file-embed-lzma.patches    = pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ./patches/file-embed-lzma-0.patch ];

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
        }
        # Fix dependencies and case-sensitive filesystem builds for unix-time.
        // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isWindows {
          unix-time.components.library.libs = [ pkgs.windows.mingw_w64_pthreads ];
          unix-time.postUnpack = "substituteInPlace */cbits/win_patch.h --replace Windows.h windows.h";
        };
      } // withTH
      )
    ];
  });
}
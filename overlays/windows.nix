# Note: We *can not* use a global `lib.optionalAttrs hostPlatform.isWindows`, the
# conditionals need to be in the leafs! If we attach the conditional to the root
# node (e.g. the whole customization here), they will be evaluated at the wrong time
# and not end up with the expected changes we want.
final: prev:
{
   # on windows we have this habit of putting libraries
   # into `bin`, whereas on unix it's usually `lib`. For
   # this confuses nix easily. So we'll just move the
   # .dll's from `bin` into `$out/lib`. Such that they
   # are trivially found.
  #  openssl = prev.openssl.overrideAttrs (drv: {
  #   #  postInstall = with prev.stdenv; drv.postInstall + lib.optionalString hostPlatform.isWindows ''
  #   #    cp $bin/bin/*.dll $out/lib/
  #   #  '';
  #   postFixup = "";
  #  });

   mfpr = prev.mfpr.overrideAttrs (drv: {
     configureFlags = (drv.configureFlags or []) ++ prev.lib.optional prev.stdenv.hostPlatform.isWindows "--enable-static --disable-shared" ;
   });

   libmpc = prev.libmpc.overrideAttrs (drv: {
     configureFlags = (drv.configureFlags or []) ++ prev.lib.optional prev.stdenv.hostPlatform.isWindows "--enable-static --disable-shared" ;
   });

   binutils-unwrapped = prev.binutils-unwrapped.overrideAttrs (attrs: {
     patches = attrs.patches ++ final.lib.optional (final.stdenv.targetPlatform.isWindows && attrs.version or "" == "2.31.1") (
       final.fetchpatch {
         name = "plugin-target-handling-patch";
         url = "https://sourceware.org/git/?p=binutils-gdb.git;a=patch;h=999d6dff80fab12d22c2a8d91923db6bde7fb3e5";
         excludes = ["bfd/ChangeLog"];
         sha256 = "0a60w52wrf6qzchsiviprmcblq0q1fv1rbkx4gkk482dmvx4j0l6";
       }
     );
   });

   haskell-nix = prev.haskell-nix // ({
     defaultModules = prev.haskell-nix.defaultModules ++ [
      ({ pkgs, buildModules, config, lib, ... }:
      let
        withTH = import ./mingw_w64.nix {
          inherit (pkgs.stdenv) hostPlatform;
          inherit (pkgs) stdenv lib writeScriptBin;
          wine = pkgs.buildPackages.winePackages.minimal;
          inherit (pkgs.windows) mingw_w64_pthreads;
          inherit (pkgs) gmp;
          inherit (pkgs.buildPackages) symlinkJoin;
          # iserv-proxy needs to come from the buildPackages, as it needs to run on the
          # build host.
          inherit (final.buildPackages.ghc-extra-packages."${config.compiler.nix-name}".iserv-proxy.components.exes) iserv-proxy;
          # remote-iserv however needs to come from the regular packages as it has to
          # run on the target host.
          inherit (final.ghc-extra-packages."${config.compiler.nix-name}".remote-iserv.components.exes) remote-iserv;
          # we need to use openssl.bin here, because the .dll's are in the .bin expression.
          # extra-test-libs = [ pkgs.rocksdb pkgs.openssl.bin pkgs.libffi pkgs.gmp ];
        } // {
          # we can perform testing of cross compiled test-suites by using wine.
          # Therefore let's enable doCrossCheck here!
          doCrossCheck = pkgs.stdenv.hostPlatform.isWindows;
        };
      in {
        packages = {

          # This is a rather bad hack.  What we *really* would want is to make
          # sure remote-iserv has access to all the relevant libraries it needs.
          # As windows looks the libraries up next to the executable, and iserv
          # ends up dynamically loading code and executing it, we need to place
          # the necessary libraries right next to it. At least those libraries
          # we need during the build.
          # This would be fixed properly in the mingw_w64.nix file by dynamically
          # figuring out which libraries we need for the build (walking the
          # dependencies) and then placing them somewhere where wine+remote-iserv
          # will find them.
          remote-iserv.postInstall = pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isWindows (
            let extra-libs = [ pkgs.openssl.bin pkgs.libffi pkgs.gmp pkgs.libsodium pkgs.windows.mcfgthreads pkgs.buildPackages.gcc.cc ]; in ''
            for p in ${lib.concatStringsSep " "extra-libs}; do
              find "$p" -iname '*.dll' -exec cp {} $out/bin/ \;
              find "$p" -iname '*.dll.a' -exec cp {} $out/bin/ \;
            done
            (
            cd $out/bin
            for l in lib*.dll; do
              ln -s "$l" "''${l#lib}"
            done
            )
          '');

          # Apply https://github.com/haskell/cabal/pull/6055
          # See also https://github.com/input-output-hk/iohk-nix/issues/136
          # Cabal.patches = [ ({ version, revision }: (if builtins.compareVersions version "3.0.0" < 0
          #   then pkgs.fetchpatch {
          #     url = "https://patch-diff.githubusercontent.com/raw/haskell/cabal/pull/6055.diff";
          #     sha256 = "145g7s3z9q8d18pxgyngvixgsm6gmwh1rgkzkhacy4krqiq0qyvx";
          #     stripLen = 1;
          #   }
          #   else null)) ];

          # clock 0.7.2 needs to be patched to support cross compilation.
          clock.patches              = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version, revision }: (if version == "0.7.2" then ./patches/clock-0.7.2.patch else null)) ];
          # nix calls this package crypto
          cryptonite-openssl.patches = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version, revision }: if version == "0.7" then ./patches/cryptonite-openssl-0.7.patch else null) ];

          # this patch seems to be rather flaky and highly dependent on
          # the network library. I think we might need to respin that in
          # a better way that doesn't just delete some code, but makes
          # the bounds checks stricter.
          # http-client.patches        = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version, revision }: if version == "0.5.14" then ./patches/http-client-0.5.14.patch else null) ];

          conduit.patches            = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version, revision }: if builtins.compareVersions version "1.3.1.1" < 0 then ./patches/conduit-1.3.0.2.patch else null) ];
          streaming-commons.patches  = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ./patches/streaming-commons-0.2.0.0.patch ];
          x509-system.patches        = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ./patches/x509-system-1.6.6.patch ];
          file-embed.patches         = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ./patches/file-embed.patch ];
          file-embed-lzma.patches    = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ./patches/file-embed-lzma-0.patch ];

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

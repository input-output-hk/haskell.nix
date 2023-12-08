# Note: We *can not* use a global `lib.optionalAttrs hostPlatform.isWindows`, the
# conditionals need to be in the leafs! If we attach the conditional to the root
# node (e.g. the whole customization here), they will be evaluated at the wrong time
# and not end up with the expected changes we want.
final: prev:
{
  # Work around for https://github.com/NixOS/nixpkgs/pull/229465
  openssl = if !prev.stdenv.hostPlatform.isWindows then prev.openssl else prev.openssl.overrideAttrs (drv: {
    nativeBuildInputs = final.lib.filter (x: x.name or "" != "make-shell-wrapper-hook") drv.nativeBuildInputs;
    postInstall = ''
      function makeWrapper () {
        echo Skipping makeWrapper
      }
    '' + drv.postInstall;
  });
} // prev.lib.optionalAttrs (prev ? mfpr) {
   mfpr = if !prev.stdenv.hostPlatform.isWindows then prev.mpfr else prev.mfpr.overrideAttrs (drv: {
     configureFlags = (drv.configureFlags or []) ++ [ "--enable-static --disable-shared" ];
   });
} // prev.lib.optionalAttrs (prev.stdenv.hostPlatform.isWindows && prev.stdenv.hostPlatform.libc == "ucrt") {
  windows = prev.windows // {
    # TODO update stdenv.cc so that the wrapper adds -D_UCRT for libc=="ucrt"
    mingw_w64_pthreads = prev.windows.mingw_w64_pthreads.overrideAttrs { CPPFLAGS = "-D_UCRT"; };
  };
} // {
   libmpc = if !prev.stdenv.hostPlatform.isWindows then prev.libmpc else prev.libmpc.overrideAttrs (drv: {
     configureFlags = (drv.configureFlags or []) ++ [ "--enable-static --disable-shared" ];
   });

   # This work around still seems to be needed for GHC <9.4 to build
   # on windows, however it does not work on nixpkgs >= 23.11 and it breaks
   # all windows builds (including GHC >9.4).  In particular `windows.mcfgthreads`
   # fails to build.

   # We are using the `glibc.version` to determine if the fix will work.
   # `prev.lib.version` (the nixpkgs version) is not granular enough
   # (all the unstable release leading up to 23.11 have the same version as 23.11)
   # and the newer version of `glibc` in nixpkgs 23.11 might be the
   # change that caused this to break.

   # For now we have set it to be left out for nixpkgs >= 23.11
   # This means if we want to cross compile for windows and
   # use nixpkgs >= 23.11, we will need to use GHC >9.4.
   
   # GHC <9.4 does not work with binutils 2.38 from newer nixpkgs.
   # GHC >=9.4 will use clang/llvm instead.
   binutils-unwrapped =
     if final.stdenv.targetPlatform.isWindows && builtins.compareVersions prev.pkgsBuildBuild.glibc.version "2.38" < 0
       then (import prev.haskell-nix.sources.nixpkgs-2111 { inherit (prev) system; })
         .pkgsCross.mingwW64.buildPackages.binutils-unwrapped
       else prev.binutils-unwrapped;

   haskell-nix = prev.haskell-nix // ({
     defaultModules = prev.haskell-nix.defaultModules ++ [
      ({ pkgs, buildModules, config, lib, ... }:
      let
        withTH = import ./mingw_w64.nix {
          inherit (pkgs.stdenv) hostPlatform;
          inherit (pkgs.pkgsBuildBuild) lib writeShellScriptBin;
          wine = pkgs.pkgsBuildBuild.winePackages.minimal;
          inherit (pkgs.windows) mingw_w64_pthreads;
          inherit (pkgs) gmp;
          inherit (pkgs.pkgsBuildBuild) symlinkJoin;
          # iserv-proxy needs to come from the buildPackages, as it needs to run on the
          # build host.
          inherit (final.haskell-nix.iserv-proxy-exes.${config.compiler.nix-name}) iserv-proxy iserv-proxy-interpreter;
        } // {
          # we can perform testing of cross compiled test-suites by using wine.
          # Therefore let's enable doCrossCheck here!
          doCrossCheck = pkgs.stdenv.hostPlatform.isWindows;
        };
      in {
        packages = {

          # Apply https://github.com/haskell/cabal/pull/6055
          # See also https://github.com/input-output-hk/iohk-nix/issues/136
          # Cabal.patches = [ ({ version }: (if builtins.compareVersions version "3.0.0" < 0
          #   then pkgs.fetchpatch {
          #     url = "https://patch-diff.githubusercontent.com/raw/haskell/cabal/pull/6055.diff";
          #     sha256 = "145g7s3z9q8d18pxgyngvixgsm6gmwh1rgkzkhacy4krqiq0qyvx";
          #     stripLen = 1;
          #   }
          #   else null)) ];

          # clock 0.7.2 needs to be patched to support cross compilation.
          clock.patches              = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version }: (if version == "0.7.2" then ./patches/clock-0.7.2.patch else null)) ];
          # nix calls this package crypto
          cryptonite-openssl.patches = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version }: if version == "0.7" then ./patches/cryptonite-openssl-0.7.patch else null) ];

          # this patch seems to be rather flaky and highly dependent on
          # the network library. I think we might need to respin that in
          # a better way that doesn't just delete some code, but makes
          # the bounds checks stricter.
          # http-client.patches        = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version }: if version == "0.5.14" then ./patches/http-client-0.5.14.patch else null) ];

          conduit.patches            = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version }: if builtins.compareVersions version "1.3.1.1" < 0 then ./patches/conduit-1.3.0.2.patch else null) ];
          streaming-commons.patches  = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ./patches/streaming-commons-0.2.0.0.patch ];
          x509-system.patches        = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ./patches/x509-system-1.6.6.patch ];
          crypton-x509-system.patches = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ./patches/crypton-x509-system.patch ];

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

          # Newer Win32 includes hsc2hs, but we can get that that from the ghc derivation and
          # if the cabal plan included hsc2hs it winds up trying to build a windows version.
          Win32.components.library.build-tools = pkgs.lib.mkForce [];
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

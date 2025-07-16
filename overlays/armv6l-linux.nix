final: prev:
let
  isLinuxCross =
       prev.haskell-nix.haskellLib.isCrossHost
    && prev.hostPlatform.isLinux
    && (prev.hostPlatform.isAarch32
     || prev.hostPlatform.isAarch64
     || prev.hostPlatform.isi686);

in
{
   haskell-nix = prev.haskell-nix // final.lib.optionalAttrs isLinuxCross ({
     templateHaskell = builtins.mapAttrs (compiler-nix-name: iserv-proxy-exes:
        import ./linux-cross.nix {
          inherit (final.stdenv) hostPlatform buildPlatform;
          inherit (final) stdenv lib;
          inherit (final.pkgsBuildBuild) writeShellScriptBin symlinkJoin;
          inherit (final.haskell-nix) haskellLib;
          qemu = final.pkgsBuildBuild.qemu;
          inherit (final) gmp;
          inherit (iserv-proxy-exes) iserv-proxy iserv-proxy-interpreter iserv-proxy-interpreter-prof;
        }) final.haskell-nix.iserv-proxy-exes;
     defaultModules = prev.haskell-nix.defaultModules ++ [
      ({ pkgs, buildModules, config, lib, ... }:
      let
        withTH = final.haskell-nix.templateHaskell.${config.compiler.nix-name};
      in prev.haskell-nix.haskellLib.addPackageKeys {
        inherit (withTH) configureFlags testWrapper;

        setupBuildFlags = map (opt: "--ghc-option=" + opt) withTH.ghcOptions
            ++ lib.optionals pkgs.stdenv.hostPlatform.isAarch32 (map (opt: "--gcc-option=" + opt) [ "-fno-pic" "-fno-plt" ])
               # Also for GHC #15275
            ++ lib.optionals pkgs.stdenv.hostPlatform.isAarch64 ["--gcc-option=-fPIC"];

        enableShared = lib.mkDefault false;

        doCrossCheck = true;

        packages = {
          # clock 0.7.2 needs to be patched to support cross compilation.
          clock.patches              = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isAarch32 [ ({ version }: (if version == "0.7.2" then ./patches/clock-0.7.2.patch else null)) ];

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
      }
      )
     ];
   });
}

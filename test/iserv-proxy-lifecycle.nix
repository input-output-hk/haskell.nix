{ pkgs, lib, compiler-nix-name, evalPackages, testSrc ? null }:

let
  build = pkgs.pkgsBuildBuild;
  proxy = pkgs.haskell-nix.iserv-proxy-exes.${compiler-nix-name}.iserv-proxy;
in lib.recurseIntoAttrs {
  # Exercise the native proxy; these tests require no target compiler or QEMU.
  meta.disabled = build.stdenv.hostPlatform.isWindows
    || pkgs.stdenv.hostPlatform.isGhcjs || pkgs.stdenv.hostPlatform.isWasm;

  run = build.runCommand "iserv-proxy-lifecycle-${compiler-nix-name}" {
    nativeBuildInputs = [ build.python3 ];
  } ''
    python3 ${./iserv-proxy-lifecycle.py} --proxy ${proxy}/bin/iserv-proxy
    touch "$out"
  '';
}

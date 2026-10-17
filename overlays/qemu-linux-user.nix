# Use only the emulator needed by cross-TH, with Linux signal compatibility
# fixes for x86_64 QEMU running under Rosetta in an ARM64 Linux guest.
{ qemu, lib, buildPlatform, qemuSuffix }:
let
  supported = buildPlatform.isLinux
    && buildPlatform.isx86_64
    && lib.versionAtLeast qemu.version "9.1"
    && lib.versionOlder qemu.version "12";
in
if supported then
  (qemu.override {
    userOnly = true;
    hostCpuTargets = [ "${qemuSuffix}-linux-user" ];
  }).overrideAttrs (old: {
    patches = (old.patches or []) ++ [ ./patches/qemu-rosetta-signals.patch ];
  })
else qemu

# Evaluate emulator selection without importing nixpkgs or building QEMU.
let
  lib = {
    versionAtLeast = actual: minimum: builtins.compareVersions actual minimum >= 0;
    versionOlder = actual: maximum: builtins.compareVersions actual maximum < 0;
  };
  select = version: isLinux: isx86_64:
    import ../overlays/qemu-linux-user.nix {
      inherit lib;
      buildPlatform = { inherit isLinux isx86_64; };
      qemuSuffix = "aarch64";
      qemu = {
        inherit version;
        original = true;
        override = options: {
          inherit options;
          overrideAttrs = change: { inherit options; attrs = change { patches = [ "upstream" ]; }; };
        };
      };
    };
  patched = version:
    let selected = select version true true;
    in selected.options.userOnly
      && selected.options.hostCpuTargets == [ "aarch64-linux-user" ]
      && builtins.head selected.attrs.patches == "upstream"
      && builtins.length selected.attrs.patches == 2;
in
assert (select "8.2" true true).original;
assert (select "12.0" true true).original;
assert (select "11.0.1" false true).original;
assert (select "11.0.1" true false).original;
assert builtins.all patched [ "9.1.3" "9.2.4" "10.1.5" "10.2.4" "11.0.1" ];
true

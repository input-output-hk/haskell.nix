# bug-fix overlay for nixpkgs:release-19.03
self: super: self.lib.optionalAttrs (self.lib.versions.majorMinor self.lib.version == "19.03") {
    #11420 (unbreak gcc on darwin)
    binutils-unwrapped = super.binutils-unwrapped.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [ self.gettext ];
    });
    #44172 TMPDIR=$NIX_BUILD_TOP is supposed to fix it, but doesn't.
    nix = super.nix.overrideAttrs (oldAttrs: {
        TMPDIR="/tmp";
        preInstallCheck = self.lib.optional self.stdenv.isDarwin ''
             export TMPDIR=/tmp
           '';
    });
}
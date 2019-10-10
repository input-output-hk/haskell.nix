# bug-fix overlay for nixpkgs:release-19.03
self: super: {
    #11420 (unbreak gcc on darwin)
    binutils-unwrapped = super.binutils-unwrapped.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [ self.gettext ];
    });
}
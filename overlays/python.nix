(self: super: {
    # See nixpkgs#51135
    python37 = super.python37.override {
        packageOverrides = python-self: python-super: {
            docutils = python-super.docutils.overrideAttrs (oldAttrs: {
                LC_CTYPE = "en_US.UTF-8";
                LANG = "en_US.UTF-8";
            });
        };
    };
})
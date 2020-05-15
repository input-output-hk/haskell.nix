(final: prev: {
    # See nixpkgs#51135
    python37 = prev.python37.override {
        packageOverrides = python-final: python-prev: {
            docutils = python-prev.docutils.overrideAttrs (oldAttrs: {
                LC_CTYPE = "en_US.UTF-8";
                LANG = "en_US.UTF-8";
            });
        };
    };
})
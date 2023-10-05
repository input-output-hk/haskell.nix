(_final: prev: {
    # See nixpkgs#51135
    python37 = prev.python37.override {
        packageOverrides = _python-final: python-prev: {
            docutils = python-prev.docutils.overrideAttrs (_oldAttrs: {
                LC_CTYPE = "en_US.UTF-8";
                LANG = "en_US.UTF-8";
            });
        };
    };
})
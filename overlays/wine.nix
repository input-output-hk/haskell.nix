final: prev: {
  winePackages = prev.winePackages // {
    minimal = prev.winePackages.minimal.overrideAttrs (oldAttrs: {
      # Fix issue with UNC file paths
      patches = oldAttrs.patches or []
        ++ final.lib.optional (builtins.compareVersions prev.winePackages.minimal.version "10.0" < 0) ./patches/wine-add-dll-directory.patch;
      # Avoid dependency on X11
      configureFlags = oldAttrs.configureFlags or [] ++ [ "--without-x" ];
    });
  };
}

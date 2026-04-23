_final: prev: {
  winePackages = prev.winePackages // {
    minimal = prev.winePackages.minimal.overrideAttrs (oldAttrs: {
      # Fix issue with UNC device file paths
      patches = oldAttrs.patches or []
        ++ [(if builtins.compareVersions prev.winePackages.minimal.version "10.0" < 0
          then ./patches/wine-add-dll-directory.patch
          else if builtins.compareVersions prev.winePackages.minimal.version "11.0" < 0
          then ./patches/wine-add-dll-directory-10.patch
          else ./patches/wine-add-dll-directory-11.patch)];
      # Avoid dependency on X11
      configureFlags = oldAttrs.configureFlags or [] ++ [ "--without-x" ];
    });
  };
  wine64Packages = prev.wine64Packages // {
    minimal = prev.wine64Packages.minimal.overrideAttrs (oldAttrs: {
      # Fix issue with UNC device file paths
      patches = oldAttrs.patches or []
        ++ [(if builtins.compareVersions prev.wine64Packages.minimal.version "10.0" < 0
          then ./patches/wine-add-dll-directory.patch
          else if builtins.compareVersions prev.wine64Packages.minimal.version "11.0" < 0
          then ./patches/wine-add-dll-directory-10.patch
          else ./patches/wine-add-dll-directory-11.patch)];
      # Avoid dependency on X11
      configureFlags = oldAttrs.configureFlags or [] ++ [ "--without-x" ];
    });
  };
}

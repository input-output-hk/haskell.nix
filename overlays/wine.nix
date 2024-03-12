# fix wine at 5.4; later versions build with ucrt and this can break TH code (for instance accessing
# files from TH code) for GHC built with msvcrt (ghc<9.6).
# This will inevitably replace *any* wine version. Thus this might not really be what we ultimately want.
# Wine 5.4 does not build on macOS so that is not pinned and TH code will probably break.
_final: prev: {
  winePackages = prev.winePackages // {
    minimal = prev.winePackages.minimal.overrideAttrs (oldAttrs: {
      patches = oldAttrs.patches or [] ++ [ ./patches/wine-add-dll-directory.patch ];
      configureFlags = oldAttrs.configureFlags or [] ++ [ "--without-x" ];
    });
  };
}

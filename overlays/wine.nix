# fix wine at 5.4; later versions build with ucrt and this can break TH code (for instance accessing
# files from TH code) for GHC built with msvcrt (ghc<9.6).
# This will inevitably replace *any* wine version. Thus this might not really be what we ultimately want.
# Wine 5.4 does not build on macOS so that is not pinned and TH code will probably break.
final: prev:
prev.lib.optionalAttrs (!prev.stdenv.hostPlatform.isDarwin) {
    winePackages = prev.winePackages // {
        minimal = prev.winePackages.minimal.overrideAttrs (oldAttrs: {
            name = "wine-5.4";
            version = "5.4";
            src = prev.fetchurl {
            url = "https://dl.winehq.org/wine/source/5.x/wine-5.4.tar.xz";
            sha256 = "sha256-Sz4rD/pUFfGZVA5gUcKMOXb86R6lv7LPSgmcJXMXBSw="; };
            patches = [];
            });
        };
}

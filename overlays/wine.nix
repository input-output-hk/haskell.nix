# fix wine at 3.0.2; the 4.x branch breaks windows cross compilation.
# this will inevitably replace *any* wine version. Thus this might not
# really be what we ultimately want.
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

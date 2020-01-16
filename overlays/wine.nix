# fix wine at 3.0.2; the 4.x branch breaks windows cross compilation.
# this will inevitably replace *any* wine version. Thus this might not
# really be waht we ultimately want.
self: super:
{
    winePackages = super.winePackages // {
        minimal = super.winePackages.minimal.overrideAttrs (oldAttrs: {
            name = "wine-3.0.2";
            version = "3.0.2";
            src = super.fetchurl {
            url = "https://dl.winehq.org/wine/source/3.0/wine-3.0.2.tar.xz";
            sha256 = "1zv3nk31s758ghp4795ym3w8l5868c2dllmjx9245qh9ahvp3mya"; };
            });
        };
}

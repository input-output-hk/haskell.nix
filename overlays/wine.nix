# fix wine at 3.0.2; the 4.x branch breaks windows cross compilation.
# this will inevitably replace *any* wine version. Thus this might not
# really be what we ultimately want.
self: super:
{
    winePackages = super.winePackages // {
        minimal = super.winePackages.minimal.overrideAttrs (oldAttrs: {
            name = "wine-3.21";
            version = "3.21";
            src = super.fetchurl {
            url = "https://dl.winehq.org/wine/source/3.x/wine-3.21.tar.xz";
            sha256 = "1h70wb7kysbzv36i3fblyiihvalwhy6sj4s2a8nf21nz2mhc0k58"; };
            });
        };
}

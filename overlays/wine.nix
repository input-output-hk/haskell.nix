# fix wine at 3.0.2; the 4.x branch breaks windows cross compilation.
# this will inevitably replace *any* wine version. Thus this might not
# really be what we ultimately want.
self: super:
{
    winePackages = super.winePackages // {
        minimal = super.winePackages.minimal.overrideAttrs (oldAttrs: {
            name = "wine-3.0.5";
            version = "3.0.5";
            src = super.fetchurl {
            url = "https://dl.winehq.org/wine/source/3.0/wine-3.0.5.tar.xz";
            sha256 = "04g2iipcc7fnr0zfwkav6rzb6vs2gprg6mxr0r9y9av4nnar0hnw"; };
            });
        };
}

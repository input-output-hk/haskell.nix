self: super:
{
   # on windows we have this habit of putting libraries
   # into `bin`, wheras on unix it's usually `lib`. For
   # this confuses nix easily. So we'll just move the
   # .dll's from `bin` into `$out/lib`. Such that they
   # are trivially found.
   openssl = super.openssl.overrideAttrs (drv: {
    #  postInstall = with super.stdenv; drv.postInstall + lib.optionalString hostPlatform.isWindows ''
    #    cp $bin/bin/*.dll $out/lib/
    #  '';
    postFixup = "";
   });
   mfpr = super.mfpr.overrideAttrs (drv: {
     configureFlags = with super.stdenv; (drv.configureFlags or []) ++ lib.optional hostPlatform.isWindows "--enable-static --disable-shared";
   });
   libmpc = super.libmpc.overrideAttrs (drv: {
     configureFlags = with super.stdenv; (drv.configureFlags or []) ++ lib.optional hostPlatform.isWindows "--enable-static --disable-shared";
   });
}
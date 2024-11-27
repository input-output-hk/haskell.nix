final: prev: {
  # Use android SDK version 26
  lib = prev.lib // {
    systems = prev.lib.systems // {
      examples = prev.lib.systems.examples // {
        aarch64-android = prev.lib.systems.examples.aarch64-android // {
          androidSdkVersion = "26";
          androidNdkVersion = "24";
        };
        armv7a-android-prebuilt = prev.lib.systems.examples.armv7a-android-prebuilt // {
          androidSdkVersion = "26";
          androidNdkVersion = "24";
        };
      };
    };
  };
} // prev.lib.optionalAttrs prev.stdenv.hostPlatform.isAndroid ({

  # we really only want the static one.
  libiconv = (prev.libiconv.override { enableStatic = true; enableShared = false; }).overrideAttrs(_: {
    hardeningDisable = [ "fortify" "stackprotector" "format" ];
    # For some reason building libiconv with nixpgks android setup produces
    # LANGINFO_CODESET to be found, which is not compatible with android sdk 23;
    # so we'll patch up iconv to not include that.
    postConfigure = ''
      echo "#undef HAVE_LANGINFO_CODESET" >> libcharset/config.h
      echo "#undef HAVE_LANGINFO_CODESET" >> lib/config.h
    '';
    # my current thinking is that this is due to the android toolchain using r23, api30.
  });
  libffi = prev.libffi.overrideAttrs ( old: {
    dontDisableStatic = true;
    configureFlags = old.configureFlags ++ [ "--disable-shared" ];

    hardeningDisable = [ "fortify" "stackprotector" "format" ];
  } // prev.lib.optionalAttrs (prev.stdenv.hostPlatform.isAarch32 || prev.stdenv.hostPlatform.isAarch64) {
    # see libiconv. We want to target a lower minsdk
    postConfigure = ''
      echo "#undef HAVE_MEMFD_CREATE" >> ${prev.stdenv.hostPlatform.config}/fficonfig.h
    '';
  });
  gmp6 = (prev.gmp6.override { withStatic = true; }).overrideAttrs(old: {
    hardeningDisable = [ "fortify" "stackprotector" "format" ];
    configureFlags = old.configureFlags ++ [ "--disable-shared" ];
  });
  zlib = prev.zlib.override { shared = false; static = true; };
  # kernel tls (ktls) doesn't work with the android kernel. And will complain
  # about lots of implicitly declared functions and undeclared identifiers,
  # because the android (linux) kernel doesn't expose those.
  openssl = prev.openssl.override { static = true; enableKTLS = false; };
  
}) // prev.lib.optionalAttrs (prev.stdenv.targetPlatform.isAndroid && (!prev.stdenv.hostPlatform.useAndroidPrebuilt)) ({
  # we still need the shared libraries to link against on the platform.  GHC
  # has been neutered to not even try loading shared libs and will use dynamic ones.
  # We also link iserv against the static libs, so that we have a fully static
  # android (bionic/linux) iserv we can execute on glibc/linux.
  bionic = prev.bionic.override { enableStatic = true; enableShared = true; };
})

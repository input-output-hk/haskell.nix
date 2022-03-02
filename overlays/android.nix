final: prev: prev.lib.optionalAttrs prev.stdenv.hostPlatform.isAndroid ({
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
  libffi = prev.libffi.overrideAttrs (_: {
    dontDisableStatic = true;

    hardeningDisable = [ "fortify" "stackprotector" "format" ];
    # see libiconv. We want to target a lower minsdk
    postConfigure = ''
      echo "#undef HAVE_MEMFD_CREATE" >> aarch64-unknown-linux-android/fficonfig.h
    '';
  });
  gmp6 = (prev.gmp6.override { withStatic = true; }).overrideAttrs(_: {
    hardeningDisable = [ "fortify" "stackprotector" "format" ];
  });
  numactl = (prev.numactl.overrideAttrs (attrs: {
     patches = attrs.patches + [ ./patches/numactl-2.0.14-no-librt.patch ];
  });
}) // prev.lib.optionalAttrs prev.stdenv.targetPlatform.isAndroid ({
  bionic = prev.bionic.override { enableStatic = true; };
})

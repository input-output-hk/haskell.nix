final: prev: prev.lib.optionalAttrs prev.stdenv.hostPlatform.isAndroid ({
  # we really only want the static one.
  libiconv = prev.libiconv.override { enableStatic = true; enableShared = false; };
})

final: prev: prev.lib.optionalAttrs prev.stdenv.hostPlatform.isAndroid ({
  libiconv = prev.libiconv.override { enableStatic = true; };
})

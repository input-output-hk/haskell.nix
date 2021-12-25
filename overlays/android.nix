final: prev: prev.lib.optionalAttrs (prev.stdenv.hostPlatform.isAndroid || prev.stdenv.targetPlatform.isAndroid) ({
  libiconv = prev.libiconv.override { enableStatic = true; };
})

self: super: super.lib.optionalAttrs super.stdenv.hostPlatform.isMusl {
  # musl appears to need a static openssl to build
  openssl = (super.openssl.override { static = true; })
    .overrideAttrs (attrs: {
      configureScript = "./Configure linux-x86_64";
      configureFlags = attrs.configureFlags
        ++ super.lib.optional (super.lib.versionAtLeast attrs.version "1.1.0") "no-shared";
    });
}

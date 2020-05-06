final: prev: prev.lib.optionalAttrs prev.stdenv.hostPlatform.isMusl {
  # On nixpkgs 19.09 openssl is configured as `linux-generic64` instead
  # of `linux-x86_64` and as a result the `asm` parts of of openssl
  # are not built.  Because the `no_asm` configure flag is also not passed
  # the c versions of the functions are also not included.
  openssl = prev.openssl.overrideAttrs (attrs:
    prev.lib.optionalAttrs prev.stdenv.hostPlatform.isx86_64 {
      configureScript = "./Configure linux-x86_64";
    });
}

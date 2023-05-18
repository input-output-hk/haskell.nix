final: prev: prev.lib.optionalAttrs prev.stdenv.hostPlatform.isMusl ({
  # Prevent pkgsMusl.pkgsStatic chain
  busybox-sandbox-shell = final.busybox-sandbox-shell.override { inherit (final) busybox; };

  # we don't want the static output to be split. That just
  # messes with the z -> libz mapping. We can't have a conditional
  # z -> libz / z -> libz.static mapping without threading the
  # package configuration in.  That seems a bit overkill.
  zlib = final.zlib.override { splitStaticOutput = false; };

  # and a few more packages that need their static libs explicitly enabled
  bzip2 = final.bzip2.override { linkStatic = true; };
  gmp = final.gmp.override { withStatic = true; };
  ncurses = final.ncurses.override { enableStatic = true; };
  libsodium = final.libsodium.overrideAttrs (_: { dontDisableStatic = true; });
  zstd = final.zstd.override { static = true; };
  xz = final.xz.override { enableStatic = true; };
  lzma = final.lzma.override { enableStatic = true; };
  pcre = final.pcre.overrideAttrs (_: { dontDisableStatic = true; });
  secp256k1 = final.secp256k1.overrideAttrs ( oldAttrs: {
    configureFlags = oldAttrs.configureFlags ++ ["--enable-static"];  });

  numactl = final.numactl.overrideAttrs (_: { configureFlags = ["--enable-static"];});

  # See https://github.com/input-output-hk/haskell.nix/issues/948
  postgresql = (final.postgresql.overrideAttrs (old: { dontDisableStatic = true; }))
    .override { enableSystemd = false; gssSupport = false; };
  
  openssl = final.openssl.override { static = true; };

  icu = (final.icu.overrideAttrs (old: { configureFlags = old.configureFlags ++ [ "--enable-static" "--disable-shared" ]; }));

  # Fails on cross compile
  nix = final.nix.overrideAttrs (_: { doInstallCheck = false; });
} // prev.lib.optionalAttrs (prev.lib.versionAtLeast prev.lib.trivial.release "20.03") {
  # Fix infinite recursion between openssh and fetchcvs
  openssh = final.openssh.override { withFIDO = false; };
})

final: prev: prev.lib.optionalAttrs prev.stdenv.hostPlatform.isMusl ({
  # Prevent pkgsMusl.pkgsStatic chain
  busybox-sandbox-shell = prev.busybox-sandbox-shell.override { inherit (final) busybox; };

  # we don't want the static output to be split. That just
  # messes with the z -> libz mapping. We can't have a conditional
  # z -> libz / z -> libz.static mapping without threading the
  # package configuration in.  That seems a bit overkill.
  zlib = prev.zlib.override { splitStaticOutput = false; };

  # and a few more packages that need their static libs explicitly enabled
  bzip2 = prev.bzip2.override { linkStatic = true; };
  gmp = prev.gmp.override { withStatic = true; };
  ncurses = prev.ncurses.override { enableStatic = true; };
  libsodium = prev.libsodium.overrideAttrs (_: { dontDisableStatic = true; });
  zstd = prev.zstd.override { static = true; };
  xz = prev.xz.override { enableStatic = true; };
  lzma = prev.lzma.override { enableStatic = true; };
  pcre = prev.pcre.overrideAttrs (_: { dontDisableStatic = true; });
  secp256k1 = prev.secp256k1.overrideAttrs ( oldAttrs: {
    configureFlags = oldAttrs.configureFlags ++ ["--enable-static"];  });

  numactl = prev.numactl.overrideAttrs (_: { configureFlags = ["--enable-static"];});

  # See https://github.com/input-output-hk/haskell.nix/issues/948
  postgresql = (prev.postgresql.overrideAttrs (old: {
      dontDisableStatic = true;
      # the following is needed becuase libicu links against stdc++
      NIX_LDFLAGS = "--push-state --as-needed -lstdc++ --pop-state";
      # without this collate.icu.utf8, and foreign_data will fail.
      LC_CTYPE = "C";
    })).override { enableSystemd = false; gssSupport = false; };

  openssl = prev.openssl.override { static = true; };

  icu = (prev.icu.overrideAttrs (old: { configureFlags = old.configureFlags ++ [ "--enable-static" "--disable-shared" ]; }));

  # Fails on cross compile
  nix = prev.nix.overrideAttrs (_: { doInstallCheck = false; });
} // prev.lib.optionalAttrs (prev.lib.versionAtLeast prev.lib.trivial.release "20.03") {
  # Fix infinite recursion between openssh and fetchcvs
  openssh = prev.openssh.override { withFIDO = false; };
})

final: prev: prev.lib.optionalAttrs prev.stdenv.hostPlatform.isMusl ({
  # On nixpkgs 19.09 openssl is configured as `linux-generic64` instead
  # of `linux-x86_64` and as a result the `asm` parts of of openssl
  # are not built.  Because the `no_asm` configure flag is also not passed
  # the c versions of the functions are also not included.
  openssl = prev.openssl.overrideAttrs (attrs:
    prev.lib.optionalAttrs prev.stdenv.hostPlatform.isx86_64 {
      configureScript = "./Configure linux-x86_64";
    });

  # Prevent pkgsMusl.pkgsStatic chain
  busybox-sandbox-shell = prev.busybox-sandbox-shell.override { inherit (final) busybox; };

  # we don't want the static output to be split. That just
  # messes with the z -> libz mapping. We can't have a conditonal
  # z -> libz / z -> libz.static mapping without threading the
  # package configuration in.  That seems a bit overkill.
  zlib = prev.zlib.override { splitStaticOutput = false; };

  # and a few more packages that need their static libs explicitly enabled
  gmp = prev.gmp.override { withStatic = true; };
  ncurses = prev.ncurses.override { enableStatic = true; };

  # Fails on cross compile
  nix = prev.nix.overrideAttrs (_: { doInstallCheck = false; });
} // prev.lib.optionalAttrs (prev.lib.versionAtLeast prev.lib.trivial.release "20.03") {
  # Fix infinite recursion between openssh and fetchcvs
  openssh = prev.openssh.override { withFIDO = false; };
})

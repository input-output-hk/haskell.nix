final: prev: prev.lib.optionalAttrs prev.stdenv.hostPlatform.isMusl ({
  # Prevent pkgsMusl.pkgsStatic chain
  busybox-sandbox-shell = prev.busybox-sandbox-shell.override { inherit (final) busybox; };

  # we don't want the static output to be split. That just
  # messes with the z -> libz mapping. We can't have a conditional
  # z -> libz / z -> libz.static mapping without threading the
  # package configuration in.  That seems a bit overkill.
  zlib = prev.zlib.override { splitStaticOutput = false; };

  # and a few more packages that need their static libs explicitly enabled
  gmp = prev.gmp.override { withStatic = true; };
  ncurses = prev.ncurses.override { enableStatic = true; };
  libsodium = prev.libsodium.overrideAttrs (_: { dontDisableStatic = true; });

  numactl = prev.numactl.overrideAttrs (_: { configureFlags = "--enable-static"; });

  # Fails on cross compile
  nix = prev.nix.overrideAttrs (_: { doInstallCheck = false; });
} // prev.lib.optionalAttrs (prev.lib.versionAtLeast prev.lib.trivial.release "20.03") {
  # Fix infinite recursion between openssh and fetchcvs
  openssh = prev.openssh.override { withFIDO = false; };
})

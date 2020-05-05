self: super: super.lib.optionalAttrs super.stdenv.hostPlatform.isMusl {
  # On nixpkgs 19.09 openssl is configured as `linux-generic64` instead
  # of `linux-x86_64` and as a result the `asm` parts of of openssl
  # are not built.  Because the `no_asm` configure flag is also not passed
  # the c versions of the functions are also not included.
  openssl = super.openssl.overrideAttrs (attrs:
    super.lib.optionalAttrs super.stdenv.hostPlatform.isx86_64 {
      configureScript = "./Configure linux-x86_64";
    });

  # Fix infinite recursion between openssh and fetchcvs
  openssh = super.openssh.override { withFIDO = false; };

  # Prevent pkgsMusl.pkgsStatic chain
  busybox-sandbox-shell = super.busybox-sandbox-shell.override { inherit (self) busybox; };

  # Fails on cross compile
  nix = super.nix.overrideAttrs (_: { doInstallCheck = false; });
}

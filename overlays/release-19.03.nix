# bug-fix overlay for nixpkgs:release-19.03
self: super: super.lib.optionalAttrs (super.lib.versions.majorMinor super.lib.version == "19.03") {
    #11420 (unbreak gcc on darwin)
    binutils-unwrapped = super.binutils-unwrapped.overrideAttrs (old: {
        buildInputs = old.buildInputs
          ++ self.lib.optional self.stdenv.isDarwin self.gettext;
    });
    #44172 TMPDIR=$NIX_BUILD_TOP is supposed to fix it, but doesn't.
    nix = super.nix.overrideAttrs (oldAttrs: {
        TMPDIR="/tmp";
        preInstallCheck = self.lib.optional self.stdenv.isDarwin ''
             export TMPDIR=/tmp
           '';
    });
    openssl = if self.stdenv.hostPlatform.isWindows
      then let static = false;  # TODO check that it is ok to assume this (or find a way to detect without instanciating perl)
        in super.openssl.overrideAttrs (attrs: {
          postInstall = self.stdenv.lib.optionalString (!static) ''
            # If we're building dynamic libraries, then don't install static
            # libraries.
            if [ -n "$(echo $out/lib/*.so $out/lib/*.dylib $out/lib/*.dll)" ]; then
                rm "$out/lib/"*.a
            fi

          '' +
          ''
            mkdir -p $bin
            mv $out/bin $bin/

            mkdir $dev
            mv $out/include $dev/

            # remove dependency on Perl at runtime
            rm -r $out/etc/ssl/misc

            rmdir $out/etc/ssl/{certs,private}
          '';
        postFixup = "";
      })
      else super.openssl;
}
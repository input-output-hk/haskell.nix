# bug-fix overlay for nixpkgs:release-19.03
final: prev: prev.lib.optionalAttrs (prev.lib.versions.majorMinor prev.lib.version == "19.03") {
    #11420 (unbreak gcc on darwin)
    binutils-unwrapped = prev.binutils-unwrapped.overrideAttrs (old: {
        buildInputs = old.buildInputs
          ++ final.lib.optional final.stdenv.isDarwin final.gettext;
    });
    #44172 TMPDIR=$NIX_BUILD_TOP is supposed to fix it, but doesn't.
    nix = prev.nix.overrideAttrs (oldAttrs: {
        TMPDIR="/tmp";
        preInstallCheck = final.lib.optional final.stdenv.isDarwin ''
             export TMPDIR=/tmp
           '';
    });
    # Avoid dependency on perl for windows as it fails to build.
    openssl = if final.stdenv.hostPlatform.isWindows
      then let static = false;  # TODO check that it is ok to assume this (or find a way to detect without instanciating perl)
        in prev.openssl.overrideAttrs (attrs: {
          # This is the `postInstall` from 19.03 nixpkgs but with the line
          #   substituteInPlace $out/bin/c_rehash --replace ${buildPackages.perl} ${perl}
          # removed (as including it requires perl to build).
          # (code is copied here because modifying `attrs.postInstall` would still
          # introducing the same dependency)
          postInstall = final.stdenv.lib.optionalString (!static) ''
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
        # Skip this step on windows as it checks for references to buildPackages.perl
        # and we are keeping them.
        postFixup = "";
      })
      else prev.openssl;
    winePackages = prev.winePackages // {
      minimal = prev.winePackages.minimal.overrideAttrs (oldAttrs: {
        # Needed for CFNotificationCenterAddObserver symbols.
        buildInputs = oldAttrs.buildInputs ++ final.lib.optional final.stdenv.isDarwin
	      final.buildPackages.darwin.cf-private;
        # the relevant check in configure.ac will fail under nix as we are not
        # using the system supplied compiler tools. We will subsequently *not*
        # pass `-mmacosx-version-min` in the LDFLAGS, and end up with a linked
        # binary against 10.14 which *does* contain a __DATA,__dyld section and
        # then horribly fail to run on Mojave.
        postConfigure = final.lib.optional (final.stdenv.hostPlatform.isDarwin) ''
          sed -i 's|-nostartfiles -nodefaultlibs|-nostartfiles -nodefaultlibs -mmacosx-version-min=10.7|g' loader/Makefile
        '';
      });
    };
}
[{
  package-keys = ["HsOpenSSL" "libsodium" "double-conversion"];
  # See https://github.com/haskell-cryptography/HsOpenSSL/issues/95
  packages.HsOpenSSL.ghcOptions = ["-optc=-Wno-incompatible-pointer-types"];
}

({pkgs, lib, ...}: lib.mkIf pkgs.stdenv.hostPlatform.isAndroid {
  packages.libsodium.configureFlags = [ "--c2hs-option=--cppopts=-D_Null_unspecified=" ];
  packages.libsodium.components.library.hardeningDisable = ["fortify"];
})

({pkgs, lib, ...}: lib.mkIf pkgs.stdenv.hostPlatform.isStatic {
  packages.double-conversion.ghcOptions = [
    # stop putting U __gxx_personality_v0 into the library!
    "-optcxx-fno-rtti"
    "-optcxx-fno-exceptions"
    # stop putting U __cxa_guard_release into the library!
    "-optcxx-std=gnu++98"
    "-optcxx-fno-threadsafe-statics"
  ];
})
]

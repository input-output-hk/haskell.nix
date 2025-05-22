[{
  # See https://github.com/haskell-cryptography/HsOpenSSL/issues/95
  packages.HsOpenSSL.ghcOptions = ["-optc=-Wno-incompatible-pointer-types"];
}

({pkgs, lib, ...}: lib.mkIf pkgs.stdenv.hostPlatform.isAndroid {
  packages.libsodium.configureFlags = [ "--c2hs-option=--cppopts=-D_Null_unspecified=" ];
  packages.libsodium.components.library.hardeningDisable = ["fortify"];
})
]

[{
  # See https://github.com/haskell-cryptography/HsOpenSSL/issues/95
  packages.HsOpenSSL.ghcOptions = ["-optc=-Wno-incompatible-pointer-types"];
}]

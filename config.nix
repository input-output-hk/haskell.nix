{
  # allow building for windows
  allowUnsupportedSystem = true;
  # we want the 64bit wine version
  wine.build = "wine64";
  wine.release = "stable";

  # This is marked insecure on 20.03, but many packages still depend on it
  permittedInsecurePackages = [ "openssl-1.0.2u" ];
}


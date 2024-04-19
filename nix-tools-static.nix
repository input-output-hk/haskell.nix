pkgs: let baseurl = "https://github.com/input-output-hk/haskell.nix/releases/download/nix-tools-0.1.8/"; in {
  aarch64-darwin = pkgs.fetchurl { 
     name = "aarch64-darwin-nix-tools-static";
     url = "${baseurl}aarch64-darwin-nix-tools-static.zip";
     sha256 = "sha256-du+QOnEkjVQFHiwHr7NGOku/fmvBUCFYRPipzU/MRPc=";
  };
  x86_64-darwin = pkgs.fetchurl { 
     name = "x86_64-darwin-nix-tools-static";
     url = "${baseurl}x86_64-darwin-nix-tools-static.zip";
     sha256 = "sha256-I55Mn6iKYxy3uoQp3DyhuJOvUJ+mDP9In9DyuEaw6K4=";
  };
  aarch64-linux = pkgs.fetchurl { 
     name = "aarch64-linux-nix-tools-static";
     url = "${baseurl}aarch64-linux-nix-tools-static.zip";
     sha256 = "sha256-kx06o5tzow6PCdkV+mlQW4TQHdkEZGtSvH4sij5ZaYk=";
  };
  x86_64-linux = pkgs.fetchurl { 
     name = "x86_64-linux-nix-tools-static";
     url = "${baseurl}x86_64-linux-nix-tools-static.zip";
     sha256 = "sha256-02mBvbW5WT+0fY9n2RIbrNnMwpJIHZ5obgmFXDTz8Ds=";
  };
}

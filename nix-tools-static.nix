pkgs: let baseurl = "https://github.com/input-output-hk/haskell.nix/releases/download/nix-tools-0.3.4/"; in {
  aarch64-darwin = pkgs.fetchurl { 
     name = "aarch64-darwin-nix-tools-static";
     url = "${baseurl}aarch64-darwin-nix-tools-static.zip";
     sha256 = "sha256-umzS70a8h1pigTzBJdkChEPqIk83NXfi02zIDQIDrzI=";
  };
  x86_64-darwin = pkgs.fetchurl { 
     name = "x86_64-darwin-nix-tools-static";
     url = "${baseurl}x86_64-darwin-nix-tools-static.zip";
     sha256 = "sha256-O0T0tI+vD5ZuozKvqlfQR93UO6p4P0HyPQDLvhUIChs=";
  };
  aarch64-linux = pkgs.fetchurl { 
     name = "aarch64-linux-nix-tools-static";
     url = "${baseurl}aarch64-linux-nix-tools-static.zip";
     sha256 = "sha256-J3z5WbEm96k25UyOK3kKRtecQvTQIORRhoROKFSULZI=";
  };
  x86_64-linux = pkgs.fetchurl { 
     name = "x86_64-linux-nix-tools-static";
     url = "${baseurl}x86_64-linux-nix-tools-static.zip";
     sha256 = "sha256-CKf8Drj1UOwlAg16S4kuyVp25O7al/5YzxwrLawVnwQ=";
  };
}

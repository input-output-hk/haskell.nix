pkgs: let baseurl = "https://github.com/input-output-hk/haskell.nix/releases/download/nix-tools-0.2.6/"; in {
  aarch64-darwin = pkgs.fetchurl { 
     name = "aarch64-darwin-nix-tools-static";
     url = "${baseurl}aarch64-darwin-nix-tools-static.zip";
     sha256 = "sha256-9WpTIWlpUvG3pI+tcbAMh6sMH0QO/coZrxDYWD43iq0=";
  };
  x86_64-darwin = pkgs.fetchurl { 
     name = "x86_64-darwin-nix-tools-static";
     url = "${baseurl}x86_64-darwin-nix-tools-static.zip";
     sha256 = "sha256-UUr9bo2OpLPsvHRSeO2B6DKVDVTsHepRlTqN6UZoZ2M=";
  };
  aarch64-linux = pkgs.fetchurl { 
     name = "aarch64-linux-nix-tools-static";
     url = "${baseurl}aarch64-linux-nix-tools-static.zip";
     sha256 = "sha256-96s6RXN8st0JK0eYSOkTJvnlTxYVdE81+ZUGJEsC46A=";
  };
  x86_64-linux = pkgs.fetchurl { 
     name = "x86_64-linux-nix-tools-static";
     url = "${baseurl}x86_64-linux-nix-tools-static.zip";
     sha256 = "sha256-LMFVUKNycjVFBb3ChZsPbRNgab50zOHl7nMBrDdeTrQ=";
  };
}

pkgs: let baseurl = "https://github.com/input-output-hk/haskell.nix/releases/download/nix-tools-0.4.4/"; in {
  aarch64-darwin = pkgs.fetchurl { 
     name = "aarch64-darwin-nix-tools-static";
     url = "${baseurl}aarch64-darwin-nix-tools-static.zip";
     sha256 = "sha256-ry+cIGZSx4Pv6Wqzm3OsbafjRTLQ16/4JXkX0wsoANA=";
  };
  x86_64-darwin = pkgs.fetchurl { 
     name = "x86_64-darwin-nix-tools-static";
     url = "${baseurl}x86_64-darwin-nix-tools-static.zip";
     sha256 = "sha256-PosrlUy1pDNB2ksc6c7K9LJLyGphP1YHpaq2mWYdbsI=";
  };
  aarch64-linux = pkgs.fetchurl { 
     name = "aarch64-linux-nix-tools-static";
     url = "${baseurl}aarch64-linux-nix-tools-static.zip";
     sha256 = "sha256-cVWYeXA16Y6gaj4kp28c5WH9UmJEp9+spjrkury5nfA=";
  };
  x86_64-linux = pkgs.fetchurl { 
     name = "x86_64-linux-nix-tools-static";
     url = "${baseurl}x86_64-linux-nix-tools-static.zip";
     sha256 = "sha256-RZtisku34csEy8EKh2gGgYAkggSk83EcTNjkWAkXiNc=";
  };
}

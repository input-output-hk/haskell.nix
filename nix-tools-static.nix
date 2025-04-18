pkgs: let baseurl = "https://github.com/input-output-hk/haskell.nix/releases/download/nix-tools-0.3.2/"; in {
  aarch64-darwin = pkgs.fetchurl { 
     name = "aarch64-darwin-nix-tools-static";
     url = "${baseurl}aarch64-darwin-nix-tools-static.zip";
     sha256 = "sha256-SlTAgNj3YjZpobl/aIZLI+o8EpxznW5X90UBTtHDwbw=";
  };
  x86_64-darwin = pkgs.fetchurl { 
     name = "x86_64-darwin-nix-tools-static";
     url = "${baseurl}x86_64-darwin-nix-tools-static.zip";
     sha256 = "sha256-6m2f3DoARyoxR5Fh+87TfVCLkewwhozVLKbUzfXvUxs=";
  };
  aarch64-linux = pkgs.fetchurl { 
     name = "aarch64-linux-nix-tools-static";
     url = "${baseurl}aarch64-linux-nix-tools-static.zip";
     sha256 = "sha256-MX4u23nzjByT9zcSN+HlKOAgQNtYvcuR8iOh54wR41U=";
  };
  x86_64-linux = pkgs.fetchurl { 
     name = "x86_64-linux-nix-tools-static";
     url = "${baseurl}x86_64-linux-nix-tools-static.zip";
     sha256 = "sha256-QzTCy7HPY5xN6VFKJcibE1gWLsT4u8OPfJHvDMK3v7M=";
  };
}

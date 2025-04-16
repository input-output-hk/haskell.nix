pkgs: let baseurl = "https://github.com/input-output-hk/haskell.nix/releases/download/nix-tools-0.3.1/"; in {
  aarch64-darwin = pkgs.fetchurl { 
     name = "aarch64-darwin-nix-tools-static";
     url = "${baseurl}aarch64-darwin-nix-tools-static.zip";
     sha256 = "sha256-t/sxZAl/YH2vgjneIRainKlmYx2w/kXqrKGLwzDcb3Q=";
  };
  x86_64-darwin = pkgs.fetchurl { 
     name = "x86_64-darwin-nix-tools-static";
     url = "${baseurl}x86_64-darwin-nix-tools-static.zip";
     sha256 = "sha256-SG21YIF9lBX5zCJfK0WsLZrFFjZPVwzD4zl8Je7VSNQ=";
  };
  aarch64-linux = pkgs.fetchurl { 
     name = "aarch64-linux-nix-tools-static";
     url = "${baseurl}aarch64-linux-nix-tools-static.zip";
     sha256 = "sha256-/AJsmFO3xYi+CBHLXFcj0YdZ9LvPpwPyqo4OYNLsM3s=";
  };
  x86_64-linux = pkgs.fetchurl { 
     name = "x86_64-linux-nix-tools-static";
     url = "${baseurl}x86_64-linux-nix-tools-static.zip";
     sha256 = "sha256-SNzh+OYfVZYlgsyifG0ZkxMKh/zJYSrA25I7HFRbKyI=";
  };
}

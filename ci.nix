let nixpkgs1903 = builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/b978a94c8f9167fb86372ce1044a23f8df2edea0.tar.gz"; in
with (import nixpkgs1903 {});
recurseIntoAttrs {
    "release-19.03" = recurseIntoAttrs {
       x86_64-linux = recurseIntoAttrs {
            hello = with (import ./. { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-linux"; }; });
                (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
            x86_64-pc-mingw32-hello = with (import ./. { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-linux"; crossSystem = { config = "x86_64-pc-mingw32"; }; }; });
                (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
        };
        x86_64-darwin = recurseIntoAttrs {
            hello = with (import ./. { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-darwin"; }; });
                (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
            x86_64-pc-mingw32-hello = with (import ./. { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-darwin"; crossSystem = { config = "x86_64-pc-mingw32"; }; }; });
                (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
        };
    };
    tests = recurseIntoAttrs {
        x86_64-linux = recurseIntoAttrs (import ./test { nixpkgsArgs = { system = "x86_64-linux"; }; });
        x86_64-darwin = recurseIntoAttrs (import ./test { nixpkgsArgs = { system = "x86_64-darwin"; }; });
    };
    stackage = recurseIntoAttrs {
        x86_64-linux = recurseIntoAttrs (with (import ./. { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-linux"; }; });
            haskell-nix.snapshots."lts-13.29");
        x86_64-darwin = recurseIntoAttrs (with (import ./. { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-darwin"; }; });
            haskell-nix.snapshots."lts-13.29");
    };
}
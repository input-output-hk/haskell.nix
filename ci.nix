let nixpkgs1903 = builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/b978a94c8f9167fb86372ce1044a23f8df2edea0.tar.gz"; in
with (import nixpkgs1903 {});
let
    recRecurseIntoAttrs = pred: x: if pred x then recurseIntoAttrs (lib.mapAttrs (n: v: if n == "buildPackages" then v else recRecurseIntoAttrs pred v) x) else x;
in recRecurseIntoAttrs (x: lib.isAttrs x && !lib.isDerivation x) {
    "release-19.03" = {
       x86_64-linux = {
            hello = with (import ./. { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-linux"; }; });
                (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
            x86_64-pc-mingw32-hello = with (import ./. { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-linux"; crossSystem = { config = "x86_64-pc-mingw32"; }; }; });
                (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
        };
        # x86_64-darwin = {
        #     hello = with (import ./. { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-darwin"; }; });
        #         (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
        #     x86_64-pc-mingw32-hello = with (import ./. { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-darwin"; crossSystem = { config = "x86_64-pc-mingw32"; }; }; });
        #         (haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
        # };
    };
    haskell.compiler = {
        x86_64-linux = with (import ./. { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-linux"; }; });
            haskell.compiler;
        # x86_64-darwin = with (import ./. { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-darwin"; }; });
        #     haskell.compiler;
    };
    tests = {
        x86_64-linux = (import ./test { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-linux"; }; });
        # x86_64-darwin = (import ./test { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-darwin"; }; });
    };
    # Needs agent redeploy
    #
    # stackage = {
    #     x86_64-linux = (with (import ./. { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-linux"; }; });
    #         haskell-nix.snapshots."lts-13.29");
        # x86_64-darwin = (with (import ./. { nixpkgs = nixpkgs1903; nixpkgsArgs = { system = "x86_64-darwin"; }; });
        #     haskell-nix.snapshots."lts-13.29");
    # };
}
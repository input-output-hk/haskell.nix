let nixpkgs1903 = builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/b978a94c8f9167fb86372ce1044a23f8df2edea0.tar.gz"; in
{
    "release-19.03" = {
        x86_64-linux = {
            hello = with (import ./. { nixpkgs = nixpkgs1903; system = "x86_64-linux"; });
                (hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
            x86_64-pc-mingw32-hello = with (import ./. { nixpkgs = nixpkgs1903; system = "x86_64-linux"; crossSystem = "x86_64-pc-mingw32"; });
                (hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
        };
        x86_64-darwin = {
            hello = with (import ./. { nixpkgs = nixpkgs1903; system = "x86_64-darwin"; });
                (hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
            x86_64-pc-mingw32-hello = with (import ./. { nixpkgs = nixpkgs1903; system = "x86_64-darwin"; crossSystem = "x86_64-pc-mingw32"; });
                (hackage-package { name = "hello"; version = "1.0.0.2";}).components.exes.hello;
        };
    };
}
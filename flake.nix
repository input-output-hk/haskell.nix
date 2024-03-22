{
  description = "Alternative Haskell Infrastructure for Nixpkgs";

  inputs = {
    nixpkgs.follows = "nixpkgs-unstable";
    nixpkgs-2003 = { url = "github:NixOS/nixpkgs/nixpkgs-20.03-darwin"; };
    nixpkgs-2105 = { url = "github:NixOS/nixpkgs/nixpkgs-21.05-darwin"; };
    nixpkgs-2111 = { url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin"; };
    nixpkgs-2205 = { url = "github:NixOS/nixpkgs/nixpkgs-22.05-darwin"; };
    nixpkgs-2211 = { url = "github:NixOS/nixpkgs/nixpkgs-22.11-darwin"; };
    nixpkgs-2305 = { url = "github:NixOS/nixpkgs/nixpkgs-23.05-darwin"; };
    nixpkgs-2311 = { url = "github:NixOS/nixpkgs/nixpkgs-23.11-darwin"; };
    # The libsodium bump in 85c6e70b555fe892a049fa3d9dce000dc23a9562 breaks th-dll tests.
    # And later it breaks in th-dll due to some change in the windows libs. We should probably
    # drop unsable.
    nixpkgs-unstable = { url = "github:NixOS/nixpkgs?rev=47585496bcb13fb72e4a90daeea2f434e2501998"; }; # nixpkgs-unstable };
    ghc910X = {
      flake = false;
      url = "git+https://gitlab.haskell.org/ghc/ghc?ref=ghc-9.10&submodules=1";
    };
    ghc911 = {
      flake = false;
      url = "git+https://gitlab.haskell.org/ghc/ghc?submodules=1";
    };
    flake-compat = { url = "github:input-output-hk/flake-compat/hkm/gitlab-fix"; flake = false; };
    "hls-1.10" = { url = "github:haskell/haskell-language-server/1.10.0.0"; flake = false; };
    "hls-2.0" = { url = "github:haskell/haskell-language-server/2.0.0.1"; flake = false; };
    "hls-2.2" = { url = "github:haskell/haskell-language-server/2.2.0.0"; flake = false; };
    "hls-2.3" = { url = "github:haskell/haskell-language-server/2.3.0.0"; flake = false; };
    "hls-2.4" = { url = "github:haskell/haskell-language-server/2.4.0.1"; flake = false; };
    "hls-2.5" = { url = "github:haskell/haskell-language-server/2.5.0.0"; flake = false; };
    "hls-2.6" = { url = "github:haskell/haskell-language-server/2.6.0.0"; flake = false; };
    hydra.url = "hydra";
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    stackage = {
      url = "github:input-output-hk/stackage.nix";
      flake = false;
    };
    nix-tools-static = {
      url = "github:input-output-hk/haskell-nix-example/nix";
      flake = false;
    };
    cabal-32 = {
      url = "github:haskell/cabal/3.2";
      flake = false;
    };
    cabal-34 = {
      url = "github:haskell/cabal/3.4";
      flake = false;
    };
    cabal-36 = {
      url = "github:haskell/cabal/3.6";
      flake = false;
    };
    cardano-shell = {
      url = "github:input-output-hk/cardano-shell";
      flake = false;
    };
    "ghc-8.6.5-iohk" = {
      type = "github";
      owner = "input-output-hk";
      repo = "ghc";
      ref = "release/8.6.5-iohk";
      flake = false;
    };
    hpc-coveralls = {
      url = "github:sevanspowell/hpc-coveralls";
      flake = false;
    };
    old-ghc-nix = {
      url = "github:angerman/old-ghc-nix/master";
      flake = false;
    };
    HTTP = {
      url = "github:phadej/HTTP";
      flake = false;
    };
    iserv-proxy = {
      url = "github:stable-haskell/iserv-proxy?ref=iserv-syms";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-unstable
    , nixpkgs-2105
    , nixpkgs-2111
    , nixpkgs-2205
    , nixpkgs-2211
    , nixpkgs-2305
    , nixpkgs-2311
    , flake-compat
    , ...
    }@inputs:
    let
      callFlake = import flake-compat;

      compiler = "ghc928";
      config = import ./config.nix;

      inherit (nixpkgs) lib;

      traceNames = prefix: builtins.mapAttrs (n: v:
        if builtins.isAttrs v
          then if v ? type && v.type == "derivation"
            then builtins.trace (prefix + n) v
            else traceNames (prefix + n + ".") v
          else v);

      traceHydraJobs = x: x // { inherit (traceNames "" x) hydraJobs; };

      # systems supported by haskell.nix
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      nixpkgsArgs = {
        inherit config;
        overlays = [ self.overlay ];
      };

      forEachSystem = lib.genAttrs systems;
      forEachSystemPkgs = f: forEachSystem (system: f self.legacyPackages.${system});

      inherit
        (import ./ci-lib.nix { inherit lib; })
        stripAttrsForHydra
        filterDerivations;

      flake = {
      inherit config;
      overlay = self.overlays.combined;
      overlays = import ./overlays { sources = inputs; };

      internal = {
        nixpkgsArgs = {
          inherit config;
          overlays = [ self.overlay ];
        };

        sources = inputs;

        overlaysOverrideable =
          lib.warn
            "Using this attribute is deprecated. Import ${./overlays} directly or use the flake overlays output with override-inut."
            (import ./overlays);

        # Compatibility with old default.nix
        compat =
          lib.warn
            "Using this attribute is deprecated. You can pass the same arguments to ${./default.nix} instead"
            (import ./default.nix);
      };

      legacyPackages = forEachSystem (system:
        import nixpkgs {
          inherit config;
          overlays = [ self.overlay ];
          localSystem = { inherit system; };
        });

      legacyPackagesUnstable = forEachSystem (system:
        import nixpkgs-unstable {
          inherit config;
          overlays = [ self.overlay ];
          localSystem = { inherit system; };
        });

      # Exposed so CI can check that `allow-import-from-derivation=false` works
      # for core of haskell.nix E.g. this should always work:
      #   nix build .#roots.x86_64-linux --accept-flake-config --option allow-import-from-derivation false
      roots = forEachSystem (system:
        self.legacyPackagesUnstable.${system}.haskell-nix.roots compiler);

      # Note: `nix flake check` evaluates outputs for all platforms, and haskell.nix
      # uses IFD heavily, you have to have the ability to build for all platforms
      # supported by haskell.nix, e.g. with remote builders, in order to check this flake.
      # If you want to run the tests for just your platform, run `./test/tests.sh` or
      # `nix-build -A checks.$PLATFORM`
      checks = forEachSystemPkgs (pkgs:
        builtins.listToAttrs (
          map
            (pkg: { name = pkg.name; value = pkg; })
            (lib.collect
              lib.isDerivation
              (import ./test {
                haskellNix.sources = inputs;
                haskellNix.nixpkgsArgs = nixpkgsArgs;
                compiler-nix-name = compiler;
                inherit pkgs;
              })
              )
            )
        );

      # NOTE: these are the hix cli utilities, which is a separate thing from
      # the hix.nix overlays (which extends haskell.nix with hixProject).
      packages = forEachSystemPkgs (pkgs:
        (import ./hix/default.nix { inherit pkgs; }).apps
      );

      allJobs = forEachSystem (system:
        stripAttrsForHydra (filterDerivations (
          # This is awkward.
          import ./ci.nix {
            inherit system;
            haskellNix = self;
          })));

      requiredJobs = forEachSystem (system:
        let
          inherit (self.legacyPackages.${system}) releaseTools;
        in
          lib.concatMapAttrs (nixpkgsVer:
            lib.concatMapAttrs (compiler-nix-name:
              lib.concatMapAttrs (crossPlatform: ghcJobs:
                let
                  name = "required-${nixpkgsVer}-${compiler-nix-name}-${crossPlatform}";
                  value = releaseTools.aggregate {
                      name = "haskell.nix-${nixpkgsVer}-${compiler-nix-name}-${crossPlatform}";
                      meta.description = "All ${nixpkgsVer} ${compiler-nix-name} ${crossPlatform} jobs";
                      constituents = lib.collect lib.isDerivation ghcJobs;
                  };
                in
                lib.optionalAttrs
                  (crossPlatform != "recurseForDerivations" && crossPlatform != "meta")
                  { ${name} = value; })
              )
            )
            self.allJobs.${system}
          );

      hydraJobs = forEachSystem (system:
        let
          # Include hydraJobs from nix-tools subflake.
          # NOTE: These derivations do not depend on the haskell.nix in ./. but
          # on the version of haskell.nix locked in the subflake. They are
          # evaluated within their own flake and independently of anything
          # else. Here we only expose them in the main flake.
          nix-tools-hydraJobs =
            let cf = callFlake {
              inherit system;
              pkgs = self.legacyPackages.${system};
              src = ./nix-tools;
              override-inputs = {
                # Avoid downloading another `hackage.nix`.
                inherit (inputs) hackage;
              };
            };
            in cf.defaultNix.hydraJobs;
        in
        self.allJobs.${system} // { nix-tools = nix-tools-hydraJobs.${system} or {}; }
      );

      devShells = forEachSystemPkgs (pkgs:
        let inherit (pkgs) mkShell nixUnstable cabal-install haskell-nix;
        in {
          default =
            mkShell {
              buildInputs = [
                nixUnstable
                cabal-install
                haskell-nix.compiler.${compiler}
              ];
            };
        }
        //
        builtins.mapAttrs
          (compiler-nix-name: compiler:
            mkShell {
              buildInputs = [
                compiler
                haskell-nix.cabal-install.${compiler-nix-name}
              ];
            })
          ( # Exclude old versions of GHC to speed up `nix flake check`
            builtins.removeAttrs haskell-nix.compiler
              [ "ghc844"
                "ghc861" "ghc862" "ghc863" "ghc864"
                "ghc881" "ghc882" "ghc883"
                "ghc8101" "ghc8102" "ghc8103" "ghc8104" "ghc8105" "ghc8106" "ghc810420210212"
                "ghc901"
                "ghc921" "ghc922" "ghc923"])
      );
    }; in with (import nixpkgs { system = "x86_64-linux"; });
          traceHydraJobs (lib.recursiveUpdate flake {
            hydraJobs.nix-tools = pkgs.releaseTools.aggregate {
              name = "nix-tools";
              constituents = [
                "aarch64-darwin.nix-tools-static"
                "x86_64-darwin.nix-tools-static"
                "x86_64-linux.nix-tools-static"
                "x86_64-linux.nix-tools-static-arm64"
                "aarch64-darwin.nix-tools-static-no-ifd"
                "x86_64-darwin.nix-tools-static-no-ifd"
                "x86_64-linux.nix-tools-static-no-ifd"
                "x86_64-linux.nix-tools-static-arm64-no-ifd"
                (writeText "gitrev" (self.rev or "0000000000000000000000000000000000000000"))
              ];
            };
            # hydraJobs.all-nix-tools = runCommand "all-nix-tools" {
            #   } ''
            #   mkdir $out
            #   cp ${flake.hydraJobs.aarch64-darwin.nix-tools-static-no-ifd}/*.zip $out/
            #   cp ${flake.hydraJobs.x86_64-darwin.nix-tools-static-no-ifd}/*.zip $out/
            #   cp ${flake.hydraJobs.x86_64-linux.nix-tools-static-no-ifd}/*.zip $out/
            #   cp ${flake.hydraJobs.x86_64-linux.nix-tools-static-arm64-no-ifd}/*.zip $out/
            # '';
          });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}

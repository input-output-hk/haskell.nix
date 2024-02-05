inputs@{ nixpkgs, haskellNix, iohkNix, ... }:

let

  inherit (nixpkgs) lib;


  supported-systems = [
    "x86_64-linux"
    "x86_64-darwin"
    "aarch64-linux"
    "aarch64-darwin"
  ];


  static-libraries-overlay = final: prev: {
    static-libsodium-vrf = final.libsodium-vrf.overrideDerivation (old: {
      configureFlags = old.configureFlags ++ [ "--disable-shared" ];
    });
    static-secp256k1 = final.secp256k1.overrideDerivation (old: {
      configureFlags = old.configureFlags ++ [ "--enable-static" "--disable-shared" ];
    });
    dyn-static-secp256k1 = final.secp256k1.overrideDerivation (old: {
      configureFlags = old.configureFlags ++ [ "--enable-static" "--enable-shared" ];
    });
    static-gmp = (final.gmp.override { withStatic = true; }).overrideDerivation (old: {
      configureFlags = old.configureFlags ++ [ "--enable-static" "--disable-shared" ];
    });
    static-libblst = (final.libblst.override { enableShared = false; }).overrideDerivation (old: {
      postFixup = "";
    });
    static-openssl = (final.openssl.override { static = true; });
    static-zlib = final.zlib.override { shared = false; };
    static-pcre = final.pcre.override { shared = false; };
  };


  # This sets up the `pkgs`, by importing the nixpkgs flake and adding the 
  # haskellNix overlay. We need the iohkNix overlays to get the necessary cryto 
  # packages: secp256k1, blst, and libsodium.
  mkNixpkgsForSystem = system: import nixpkgs {

    inherit system;

    # Also ensure we are using haskellNix config. Otherwise we won't be
    # selecting the correct wine version for cross compilation.
    inherit (haskellNix) config;

    overlays = with inputs; [
      iohkNix.overlays.crypto
      haskellNix.overlay
      iohkNix.overlays.haskell-nix-extra
      iohkNix.overlays.haskell-nix-crypto
      iohkNix.overlays.cardano-lib
      iohkNix.overlays.utils
      static-libraries-overlay
      (import ./packaging.nix)
    ];
  };


  # Keep it simple (from https://ayats.org/blog/no-flake-utils/)
  forAllSystems = f:
    nixpkgs.lib.genAttrs supported-systems 
      (system: f (mkNixpkgsForSystem system));


  outputs = {
    hydraJobs = forAllSystems (pkgs: {
      zipped = import ./zipped.nix inputs pkgs;
      flake = (import ./project.nix inputs pkgs).flake'.hydraJobs;
    });
  };

in

  outputs 

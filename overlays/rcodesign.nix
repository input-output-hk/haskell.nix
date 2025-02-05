# nixpkgs is using `rcodesign` 0.22 and it fails to build on recent
# versions of macOS (one of the tests fails validating signatures
# in `/usr/bin`).
final: prev: {
  rcodesign = prev.rcodesign.override (old: final.lib.optionalAttrs (prev.rcodesign.version == "0.22.0") {
    rustPlatform = old.rustPlatform // {
      buildRustPackage = args: old.rustPlatform.buildRustPackage (args // {
        version = "0.27.0";

        src = final.fetchFromGitHub {
          owner = "hamishmack";
          repo = "apple-platform-rs";
          rev = "hkm/cargo-update";
          hash = "sha256-gma2e73m2MDC8BAcIuclG/RPLhAHRLkehCa56f5835g=";
        };

        cargoHash = "sha256-4ra1oBQK/kXZTKvvq17kX2+49iKyXXT484Z6ON4bFbU=";

        buildInputs = final.lib.optionals final.stdenv.hostPlatform.isDarwin [
          final.darwin.apple_sdk_11_0.frameworks.Security
          final.darwin.apple_sdk_11_0.frameworks.SystemConfiguration
        ];

        checkFlags = [
          # Does network IO
          "--skip=ticket_lookup::test::lookup_ticket"
          "--skip=cli_tests"
        ];
      });
    };
  });
}

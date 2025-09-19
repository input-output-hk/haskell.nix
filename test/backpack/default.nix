{ stdenv
, lib
, fetchFromGitHub
, cabalProject'
, haskellLib
, recurseIntoAttrs
, compiler-nix-name
, evalPackages
, ...
}:


let
  project = cabalProject' {
    inherit compiler-nix-name evalPackages;
    src = fetchFromGitHub {
      owner = "ekmett";
      repo = "unpacked-containers";
      rev = "7dc56993a57511b58257b5d389473e638a7082d2";
      hash = "sha256-BBsTF7H4jdZk/huvsoQWNkR3GTNgZy8mqs76pKG4Mu4=";
    };
    cabalProjectLocal = ''
      allow-newer:
        , unpacked-containers:*
        , unpacked-unordered-containers:*
    '';

  };

  packages = haskellLib.selectProjectPackages project.hsPkgs;
  components = lib.concatMap haskellLib.getAllComponents (lib.attrValues packages);

in
recurseIntoAttrs {
  ifdInputs = {
    plan-nix = lib.addMetaAttrs
      {
        platforms = lib.platforms.all;
        # Making this work for cross compilers will be difficult.
        disabled = stdenv.buildPlatform != stdenv.hostPlatform;
      }
      project.plan-nix;
  };

  run = stdenv.mkDerivation {
    name = "backpack-test";

    buildCommand = ''
      printf ${lib.concatStringsSep " " components}
      touch $out
    '';

    meta = {
      platforms = lib.platforms.all;
    };

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
}

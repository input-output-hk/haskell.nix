{ stdenv, lib, stackProject', recurseIntoAttrs, haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = stackProject' {
    inherit evalPackages;
    src = testSrc "ghc-options";
  };
  packages = project.hsPkgs;

  # Get the names of all packages. This is a test to see
  # whether there is a broken "$locals" package present.
  hasIdentifier = p: p != null && p ? identifier;
  packageNames = mapAttrsToList (name: p:
    # TODO work out why these are `ghcide` and `hnix` are here in the first place
    # it might be because we have patches in `modules/configuration.nix`
    lib.optionalString (!__elem name ["ghcide" "hnix"]) p.identifier.name)
      (filterAttrs (_name: hasIdentifier) packages);

in recurseIntoAttrs {
  # This test is somehow broken for ghcjs
  meta.disabled = stdenv.hostPlatform.isGhcjs || compiler-nix-name != "ghc984";

  ifdInputs = {
    inherit (project) stack-nix;
  };
  run = stdenv.mkDerivation {
    name = "callStackToNix-test";

    buildCommand = ''
      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.test-ghc-options.components.exes.test-ghc-options-exe}/test-stdout

      echo '${concatStringsSep " " packageNames}' > $out
    '';

    meta = rec {
      platforms = lib.platforms.all;
      broken = stdenv.hostPlatform.isGhcjs && __elem compiler-nix-name ["ghc961"];
      disabled = broken;
    };

    passthru = {
      # Attributes used for debugging with nix repl
      inherit project packages;
    };
  };
}

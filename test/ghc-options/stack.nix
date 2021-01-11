{ stdenv, stackProject', recurseIntoAttrs, haskellLib, testSrc }:

with stdenv.lib;

let
  project = stackProject' {
    src = testSrc "ghc-options";
  };
  packages = project.hsPkgs;

  # Get the names of all packages. This is a test to see
  # whether there is a broken "$locals" package present.
  hasIdentifier = p: p != null && p ? identifier;
  packageNames = mapAttrsToList (name: p: p.identifier.name) (filterAttrs (name: hasIdentifier) packages);

in recurseIntoAttrs {
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

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit project packages;
    };
  };
}

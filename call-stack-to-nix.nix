/* The function obtained when this is applied to a package set calls 
 * the stack-to-nix tool on a supplied source set and then 
 * imports the resulting pkgs.nix. The application of this function
 * to a source path can thus be used directly as the input to mkStackPackageSet
 */ 
{ nix-tools pkgs }: { src, stackYaml ? null }:
let
  pkgsNix = stdenv.mkDerivation {
    name = "${pkgs-nix";
    inherit src;
    nativeBuildInputs = [ nix-tools pkgs.nix-prefetch-git pkgs.rsync ];
    installPhase = ''
      export LANG=C.utf8 # Needed or unicode in cabal files will kill stack-to-nix
      mkdir -p $out
      stack-to-nix --stack-yaml=$src/${if stackYaml == null then "stack.yaml" else stackYaml} -o $out
      mv $out/pkgs.nix $out/default.nix
    '';
  };
in import pkgsNix

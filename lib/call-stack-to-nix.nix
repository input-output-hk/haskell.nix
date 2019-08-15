/* The function obtained when this is applied to a package set calls
 * the stack-to-nix tool on a supplied source set and then
 * imports the resulting pkgs.nix. The application of this function
 * to a source path can thus be used directly as the input to mkStackPackageSet
 *
 * see also `call-cabal-project-to-nix`!
 */
{ runCommand, nix-tools, pkgs }:
{ src, stackYaml ? null, ignorePackageYaml ? false }:
let 
  stackToNixArgs = builtins.concatStringsSep " " [
    "--stack-yaml=${src}/${if stackYaml == null then "stack.yaml" else stackYaml}"
    (if ignorePackageYaml then "--ignore-package-yaml" else "")
    "-o ."
  ];
  stack = runCommand "stack-to-nix-pkgs" {
    nativeBuildInputs = [ nix-tools pkgs.nix-prefetch-git ];
  } ''
    export LANG=C.utf8 # Needed or stack-to-nix will die on unicode inputs
    mkdir -p $out

    (cd $out && stack-to-nix ${stackToNixArgs})

    # We need to strip out any references to $src, as those won't
    # be accessable in restricted mode.
    for nixf in $(find $out -name "*.nix" -type f); do
      substituteInPlace $nixf --replace "${src}" "."
    done

    # move pkgs.nix to default.nix ensure we can just nix `import` the result.
    mv $out/pkgs.nix $out/default.nix
  '';
in 
runCommand "stack-to-nix-pkgs-with-src" { nativeBuildInputs = [ pkgs.rsync ]; } ''
  mkdir $out
  # todo: should we clean `src` to drop any .git, .nix, ... other irelevant files?
  rsync -a "${src}/" "$out/"
  rsync -a ${stack}/ $out/
  # Rsync will have made $out read only and that can cause problems when
  # nix sandboxing is enabled (since it can prevent nix from moving the directory
  # out of the chroot sandbox).  
  chmod +w $out
''

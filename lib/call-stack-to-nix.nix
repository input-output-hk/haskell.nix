/* The function obtained when this is applied to a package set calls
 * the stack-to-nix tool on a supplied source set and then
 * imports the resulting pkgs.nix. The application of this function
 * to a source path can thus be used directly as the input to mkStackPackageSet
 *
 * see also `call-cabal-project-to-nix`!
 */
{ runCommand, nix-tools, pkgs, mkCacheFile, materialize }:
{ src, stackYaml ? null, ignorePackageYaml ? false, cache ? null
, stack-sha256 ? null
, materialized ? null # Location of a materialized copy of the nix files
, checkMaterialization ? null # If true the nix files will be generated used to check plan-sha256 and material
, ... }:
let
  stackToNixArgs = builtins.concatStringsSep " " [
    "--full"
    "--stack-yaml=${src}/${if stackYaml == null then "stack.yaml" else stackYaml}"
    (if ignorePackageYaml then "--ignore-package-yaml" else "")
    "-o ."
  ];
  stack = materialize ({
    inherit materialized;
    sha256 = stack-sha256;
    sha256Arg = "stack-sha256";
    reasonNotSafe = null;
  } // pkgs.lib.optionalAttrs (checkMaterialization != null) {
    inherit checkMaterialization;
  }) (runCommand "stack-to-nix-pkgs" {
    nativeBuildInputs = [ nix-tools pkgs.nix-prefetch-git pkgs.cacert ];
    # Needed or stack-to-nix will die on unicode inputs
    LOCALE_ARCHIVE = pkgs.lib.optionalString (pkgs.stdenv.hostPlatform.libc == "glibc") "${pkgs.glibcLocales}/lib/locale/locale-archive";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    preferLocalBuild = false;
  } (''
    mkdir -p $out
  '' + pkgs.lib.optionalString (cache != null) ''
    cp ${mkCacheFile cache}/.stack-to-nix.cache* $out
  '' + ''
    (cd $out && stack-to-nix ${stackToNixArgs})

    # We need to strip out any references to $src, as those won't
    # be accessable in restricted mode.
    for nixf in $(find $out -name "*.nix" -type f); do
      substituteInPlace $nixf --replace "${src}" "."
    done

    # move pkgs.nix to default.nix ensure we can just nix `import` the result.
    mv $out/pkgs.nix $out/default.nix
  ''));
in { projectNix = stack; inherit src; sourceRepos = []; }

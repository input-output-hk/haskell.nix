/* The function obtained when this is applied to a package set calls
 * the stack-to-nix tool on a supplied source set and then
 * imports the resulting pkgs.nix. The application of this function
 * to a source path can thus be used directly as the input to mkStackPackageSet
 *
 * see also `call-cabal-project-to-nix`!
 */
{ runCommand, pkgs, mkCacheFile, materialize, haskellLib }:
{ name ? src.name or null # optional name for better error messages
, src
, stackYaml ? "stack.yaml"
, ignorePackageYaml ? false
, cache ? null
, stack-sha256 ? null
, resolverSha256 ? null
, materialized ? null # Location of a materialized copy of the nix files
, checkMaterialization ? null # If true the nix files will be generated used to check plan-sha256 and material
, nix-tools ? pkgs.haskell-nix.nix-tools.${pkgs.haskell-nix.defaultCompilerNixNameTODO}
, ... }:
let
  inherit (haskellLib.fetchResolver {
      inherit src stackYaml resolverSha256;
    }) resolver fetchedResolver;

  subDir' = src.origSubDir or "";
  stackToNixArgs = builtins.concatStringsSep " " [
    "--full"
    "--stack-yaml=$SRC/${stackYaml}"
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
  }) (runCommand (if name == null then "stack-to-nix-pkgs" else name + "-stack-to-nix-pkgs") {
    nativeBuildInputs = [ nix-tools pkgs.nix-prefetch-git pkgs.cacert pkgs.xorg.lndir ];
    # Needed or stack-to-nix will die on unicode inputs
    LOCALE_ARCHIVE = pkgs.lib.optionalString (pkgs.stdenv.hostPlatform.libc == "glibc") "${pkgs.glibcLocales}/lib/locale/locale-archive";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    preferLocalBuild = false;
  } (''
    mkdir -p $out${subDir'}
    ${
    # If no resolver was fetched use the original stack.yaml
    if fetchedResolver == null
    then ''
      SRC=${src}
    ''
    else
      # Replace the resolver path in the stack.yaml with the fetched version
      ''
      SRC=$(mktemp -d)
      cd $SRC
      lndir -silent "${src}/." $SRC
      rm ${stackYaml}
      cp ${src}/${stackYaml} .
      chmod +w ${stackYaml}
      substituteInPlace ${stackYaml} --replace "${resolver}" "${fetchedResolver}"
    ''}
    ${pkgs.lib.optionalString (cache != null) ''
      cp ${mkCacheFile cache}/.stack-to-nix.cache* $out${subDir'}
    ''}
    (cd $out${subDir'} && stack-to-nix ${stackToNixArgs})

    # We need to strip out any references to $src, as those won't
    # be accessable in restricted mode.
    for nixf in $(find $out -name "*.nix" -type f); do
      substituteInPlace $nixf --replace "$SRC" "."
    done

    # move pkgs.nix to default.nix ensure we can just nix `import` the result.
    mv $out${subDir'}/pkgs.nix $out${subDir'}/default.nix
  ''));
in { projectNix = stack; inherit src; sourceRepos = []; }

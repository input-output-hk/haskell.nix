# The use of the actual GHC can cause significant problems:
# * For hydra to assemble a list of jobs from `components.tests` it must
#   first have GHC that will be used. If a patch has been applied to the
#   GHC to be used it must be rebuilt before the list of jobs can be assembled.
#   If a lot of different GHCs are being tests that can be a lot of work all
#   happening in the eval stage where little feedback is available.
# * Once the jobs are running the compilation of the GHC needed (the eval
#   stage already must have done it, but the outputs there are apparently
#   not added to the cache) happens inside the IFD part of cabalProject.
#   This causes a very large amount of work to be done in the IFD and our
#   understanding is that this can cause problems on nix and/or hydra.
# * When using cabalProject we cannot examine the properties of the project without
#   building or downloading the GHC (less of an issue as we would normally need
#   it soon anyway).
#
# The solution here is to capture the GHC outputs that `cabal v2-configure`
# requests and materialize it so that the real GHC is only needed
# when `checkMaterialization` is set.
{ pkgs, runCommand }:
materialized-dir:
let
  inherit (pkgs.haskell-nix) checkMaterialization;
  makeDummyGhcData = ghc: ghc // {
    dummy-ghc-data =
      let
        materialized = materialized-dir + "/dummy-ghc/${ghc.targetPrefix}${ghc.name}-${pkgs.stdenv.buildPlatform.system}"
          + pkgs.lib.optionalString (builtins.compareVersions ghc.version "8.10" < 0 && ghc.targetPrefix == "" && builtins.compareVersions pkgs.lib.version "22.05" < 0) "-old";
      in pkgs.haskell-nix.materialize ({
        sha256 = null;
        sha256Arg = "sha256";
        materialized = if __pathExists materialized
          then materialized
          else __trace "WARNING: No materialized dummy-ghc-data.  mkdir ${toString materialized}"
            null;
        reasonNotSafe = null;
      } // pkgs.lib.optionalAttrs (checkMaterialization != null) {
        inherit checkMaterialization;
      }) (
    runCommand ("dummy-data-" + ghc.name) {
      nativeBuildInputs = [ ghc ];
    } ''
      mkdir -p $out/ghc
      mkdir -p $out/ghc-pkg
      ${ghc.targetPrefix}ghc --version > $out/ghc/version
      ${ghc.targetPrefix}ghc --numeric-version > $out/ghc/numeric-version
      ${ghc.targetPrefix}ghc --info | grep -v /nix/store > $out/ghc/info
      ${ghc.targetPrefix}ghc --supported-languages > $out/ghc/supported-languages
      ${ghc.targetPrefix}ghc-pkg --version > $out/ghc-pkg/version
      ${pkgs.lib.optionalString (ghc.targetPrefix == "js-unknown-ghcjs-") ''
        ${ghc.targetPrefix}ghc --numeric-ghc-version > $out/ghc/numeric-ghc-version
        ${ghc.targetPrefix}ghc --numeric-ghcjs-version > $out/ghc/numeric-ghcjs-version
        ${ghc.targetPrefix}ghc-pkg --numeric-ghcjs-version > $out/ghc-pkg/numeric-ghcjs-version
      ''}
      # The order of the `ghc-pkg dump` output seems to be non
      # deterministic so we need to sort it so that it is always
      # the same.
      # Sort the output by spliting it on the --- separator line,
      # sorting it, adding the --- separators back and removing the
      # last line (the trailing ---)
      ${ghc.targetPrefix}ghc-pkg dump --global -v0 \
        | grep -v /nix/store \
        | grep -v '^abi:' \
        | tr '\n' '\r' \
        | sed -e 's/\r\r*/\r/g' \
        | sed -e 's/\r$//g' \
        | sed -e 's/\r---\r/\n/g' \
        | sort \
        | sed -e 's/$/\r---/g' \
        | tr '\r' '\n' \
        | sed -e '$ d' \
          > $out/ghc-pkg/dump-global
    '');
  } // pkgs.lib.optionalAttrs (ghc ? dwarf) {
    dwarf = makeDummyGhcData ghc.dwarf;
  } // pkgs.lib.optionalAttrs (ghc ? smallAddressSpace) {
    smallAddressSpace = makeDummyGhcData ghc.smallAddressSpace;
  };
in makeDummyGhcData

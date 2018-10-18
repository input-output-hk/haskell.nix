{ lib, expr, mkDerivation }:
let
  # utilities
  collectAttr = a: s: lib.lists.fold (e: acc: (if e ? ${a} then e.${a} else []) ++ acc) [] (builtins.attrValues s);

  # resolver for cabal license to nix license
  resolve = { license = lib: license: (import ../lib/cabal-licenses.nix lib).${license}; };
#in mkDerivation ({
# These are the keys that <pkg>.override can override.
# the ... is used to allow to override all potential
# other keys, that the builder understands.
# { mkDerivation, stdenv, flags ? {}, hsPkgs ? {}, ... }@args:
#let
    pname = expr.package.identifier.name;
    pversion = expr.package.identifier.version;
    builderArgs = {
      inherit pname;
      version = pversion;

      isLibrary = builtins.hasAttr pname expr.components;
      isExecutable = builtins.hasAttr "exes" expr.components;

      homepage    = expr.package.homepage;
      description = expr.package.synopsis;
      license     = resolve.license lib expr.package.license;

      configureFlags = lib.mapAttrsToList (flag: enabled: (if enabled then "-f" else "-f-") + flag) expr.flags;

    } // lib.optionalAttrs (expr ? revision) {
      revision = "${toString expr.revision}";
      editedCabalFile = expr.revisionSha256;
    } // lib.optionalAttrs (expr ? sha256) {
      inherit (expr) sha256;
    } // lib.optionalAttrs (builtins.hasAttr pname expr.components) {
      libraryHaskellDepends = expr.components.${pname}.depends or [];
      libraryPkgconfigDepends = expr.components.${pname}.pkgconfig or [];
      librarySystemDepends = (expr.components.${pname}.libs or []); # ++ lib.optionals (os == "Osx") (expr.components.${pname}.frameworks or []);
      libraryToolDepends   = expr.components.${pname}.build-tools or [];
    } // lib.optionalAttrs (builtins.hasAttr "exes" expr.components) {
      executableHaskellDepends = collectAttr "depends" expr.components.exes;
      executableToolDepends    = collectAttr "build-tools" expr.components.exes;
    } // lib.optionalAttrs (builtins.hasAttr "tests" expr.components) {
      testHaskellDepends = collectAttr "depends" expr.components.tests;
    } // lib.optionalAttrs (builtins.hasAttr "benchmarks" expr.components) {
      benchmarkHaskellDepends = collectAttr "depends" expr.components.benchmarks;
    } // lib.optionalAttrs (expr ? src) {
      inherit (expr) src;
    } // lib.optionalAttrs (expr ? postUnpack) {
      inherit (expr) postUnpack;
    }
    #// builtins.removeAttrs args [ "mkDerivation" "stdenv" "flags" "hsPkgs" ];
    ;
in mkDerivation (builderArgs)
#// lib.optionalAttrs (expr.cabal-generator or "" == "hpack")
#                                { preConfigure = "hpack;" + (builderArgs.preConfigure or "");
#                                  libraryToolDepends = builderArgs.libraryToolDepends ++ [ pkgs.haskellPackages.hpack ]; })

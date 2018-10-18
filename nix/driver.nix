{ lib, expr, mkDerivation }:

let
  # utilities
  collectAttr = a: s: lib.lists.fold (e: acc: (if e ? ${a} then e.${a} else []) ++ acc) [] (builtins.attrValues s);
  pname = expr.package.identifier.name;
in mkDerivation ({
  inherit pname;
  version = expr.package.identifier.version;

  isLibrary = builtins.hasAttr pname expr.components;
  isExecutable = builtins.hasAttr "exes" expr.components;

  homepage    = expr.package.homepage;
  description = expr.package.synopsis;
  license     = (import ./cabal-licenses.nix lib).${expr.package.license};

} // lib.optionalAttrs (expr ? revision) {
  revision = "${toString expr.revision}";
  editedCabalFile = expr.revisionSha256;
} // lib.optionalAttrs (expr ? sha256) {
  inherit (expr) sha256;
} // lib.optionalAttrs (builtins.hasAttr pname expr.components) {
  libraryHaskellDepends = expr.components.${pname}.depends;
} // lib.optionalAttrs (builtins.hasAttr "exes" expr.components) {
  executableHaskellDepends = collectAttr "depends" expr.components.exes;
} // lib.optionalAttrs (builtins.hasAttr "tests" expr.components) {
  testHaskellDepends = collectAttr "depends" expr.components.tests;
} // lib.optionalAttrs (builtins.hasAttr "benchmarks" expr.components) {
  benchmarkHaskellDepends = collectAttr "depends" expr.components.benchmarks;
})

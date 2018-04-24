{ cabalexpr, pkgs, compiler ? "Ghc", version, os ? "Osx", arch ? "X86_64" }:
assert (builtins.elem compiler ["Ghc" "Ghcjs" "Nhc" "Yhc" "Hugs" "Hbc" "Helium" "Jhc" "Lhc" "Uhc" "Eta"]);
assert (builtins.elem os       ["Linux" "Windows" "Osx" "FreeBSD" "OpenBSD" "NetBSD" "DragonFly" "Solaris" "AIX" "HPUX" "IRIX" "HaLVM" "Hurd" "IOS" "Android" "Ghcjs"]);
assert (builtins.elem arch     ["I386" "X86_64" "PPC" "PPC64" "Sparc" "Arm" "Aarch64" "Mips" "SH" "IA64" "S390" "Alpha" "Hppa" "Rs6000" "M68k" "Vax" "JavaScript"]);

with rec {
  # utilities
  collectAttr = a: s: pkgs.lib.lists.fold (e: acc: (if e ? ${a} then e.${a} else []) ++ acc) [] (builtins.attrValues s);

  # resolver for cabal license to nix license
  resolve = { license = stdenv: license: (import ./cabal-licenses.nix stdenv).${license}; };

  # cabal os, arch and compilers.
  cabal = import ./cabal-os-arch-comp.nix;

  expr0 = flags: let e = cabalexpr {
    inherit flags;
    # null out self references. Tests, Benchmarks, ...
    # naturally depend on the package (as a library), but
    # we don't want those dependencies.
    hsPkgs = pkgs.haskellPackages // { ${e.package.identifier.name} = null; };
    # We also need to do some system pkgs -> haskell pkgs
    # resolution.
    pkgs = pkgs // { };
    # package-conf mappings.
    # pkgconfig name -> nix package
    pkgconfPkgs = { libpcre = pkgs.pcre; };

    compiler = cabal.compiler // {
      "is${compiler}" = true;
      version = rec {
        eq = v: builtins.compareVersions version v == 0;
        lt = v: builtins.compareVersions version v == -1;
        gt = v: builtins.compareVersions version v == 1;
        le = v: eq v || lt v;
        ge = v: eq v || gt v;
      };
    };
    system = cabal.os // { "is${os}" = true; }
          // cabal.arch // { "is${arch}" = true; };
  }; in e;
};
{ mkDerivation, stdenv, flags ? {} }:
let expr  = expr0 flags;
    pname = expr.package.identifier.name;
in mkDerivation ({
  inherit pname;
  version = expr.package.identifier.version;
  sha256 = null;

  isLibrary = builtins.hasAttr pname expr.components;
  isExecutable = builtins.hasAttr "exes" expr.components;

  homepage    = expr.package.homepage;
  description = expr.package.synopsis;
  license     = resolve.license stdenv expr.package.license;

  configureFlags = pkgs.lib.mapAttrsToList (flag: enabled: (if enabled then "-f" else "-f-") + flag) expr.flags;
} // pkgs.lib.optionalAttrs (builtins.hasAttr pname expr.components) {
  libraryHaskellDepends = expr.components.${pname}.depends;
  libraryPkgconfigDepends = expr.components.${pname}.pkgconfig or [];
  librarySystemDepends = expr.components.${pname}.libs or [];
  libraryToolDepends   = expr.components.${pname}.build-tools or [];
} // pkgs.lib.optionalAttrs (builtins.hasAttr "exes" expr.components) {
  executableHaskellDepends = collectAttr "depends" expr.components.exes;
} // pkgs.lib.optionalAttrs (builtins.hasAttr "tests" expr.components) {
  testHaskellDepends = collectAttr "depends" expr.components.tests;
} // pkgs.lib.optionalAttrs (builtins.hasAttr "benchmarks" expr.components) {
  benchmarkHaskellDepends = collectAttr "depends" expr.components.benchmarks;
} // pkgs.lib.optionalAttrs (builtins.hasAttr "src" expr) {
  inherit (expr) src;
} // pkgs.lib.optionalAttrs (builtins.hasAttr "postUnpack" expr) {
  inherit (expr) postUnpack;
})

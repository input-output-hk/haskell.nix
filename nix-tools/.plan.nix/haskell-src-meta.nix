{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.8";
      identifier = { name = "haskell-src-meta"; version = "0.8.0.5"; };
      license = "BSD-3-Clause";
      copyright = "(c) Matt Morrow";
      maintainer = "Ben Millwood <haskell@benmachine.co.uk>";
      author = "Matt Morrow";
      homepage = "";
      url = "";
      synopsis = "Parse source to template-haskell abstract syntax.";
      description = "The translation from haskell-src-exts abstract syntax\nto template-haskell abstract syntax isn't 100% complete yet.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
          (hsPkgs."haskell-src-exts" or (builtins.throw "The Haskell package set does not contain the package: haskell-src-exts (build dependency)"))
          (hsPkgs."pretty" or (builtins.throw "The Haskell package set does not contain the package: pretty (build dependency)"))
          (hsPkgs."syb" or (builtins.throw "The Haskell package set does not contain the package: syb (build dependency)"))
          (hsPkgs."template-haskell" or (builtins.throw "The Haskell package set does not contain the package: template-haskell (build dependency)"))
          (hsPkgs."th-orphans" or (builtins.throw "The Haskell package set does not contain the package: th-orphans (build dependency)"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.8") (hsPkgs."safe" or (builtins.throw "The Haskell package set does not contain the package: safe (build dependency)"));
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs."HUnit" or (builtins.throw "The Haskell package set does not contain the package: HUnit (build dependency)"))
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."haskell-src-exts" or (builtins.throw "The Haskell package set does not contain the package: haskell-src-exts (build dependency)"))
            (hsPkgs."haskell-src-meta" or (builtins.throw "The Haskell package set does not contain the package: haskell-src-meta (build dependency)"))
            (hsPkgs."pretty" or (builtins.throw "The Haskell package set does not contain the package: pretty (build dependency)"))
            (hsPkgs."template-haskell" or (builtins.throw "The Haskell package set does not contain the package: template-haskell (build dependency)"))
            (hsPkgs."test-framework" or (builtins.throw "The Haskell package set does not contain the package: test-framework (build dependency)"))
            (hsPkgs."test-framework-hunit" or (builtins.throw "The Haskell package set does not contain the package: test-framework-hunit (build dependency)"))
            ];
          };
        "splices" = {
          depends = [
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."haskell-src-meta" or (builtins.throw "The Haskell package set does not contain the package: haskell-src-meta (build dependency)"))
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/galenhuntington/haskell-src-meta.git";
      rev = "109ee29d5fd0f4e23fdd2f80eb122d66341b64a9";
      sha256 = "08qw6y9br6fy3qkwl9v2kp38msprsq9v1ssym0fsnj2jm0vbnfrx";
      });
    }
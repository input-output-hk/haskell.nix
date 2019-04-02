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
          (hsPkgs.base)
          (hsPkgs.haskell-src-exts)
          (hsPkgs.pretty)
          (hsPkgs.syb)
          (hsPkgs.template-haskell)
          (hsPkgs.th-orphans)
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.8") (hsPkgs.safe);
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs.HUnit)
            (hsPkgs.base)
            (hsPkgs.haskell-src-exts)
            (hsPkgs.haskell-src-meta)
            (hsPkgs.pretty)
            (hsPkgs.template-haskell)
            (hsPkgs.test-framework)
            (hsPkgs.test-framework-hunit)
            ];
          };
        "splices" = { depends = [ (hsPkgs.base) (hsPkgs.haskell-src-meta) ]; };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/galenhuntington/haskell-src-meta.git";
      rev = "109ee29d5fd0f4e23fdd2f80eb122d66341b64a9";
      sha256 = "08qw6y9br6fy3qkwl9v2kp38msprsq9v1ssym0fsnj2jm0vbnfrx";
      });
    }
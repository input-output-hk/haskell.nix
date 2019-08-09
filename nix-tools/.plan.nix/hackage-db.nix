{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { install-examples = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "hackage-db"; version = "2.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Peter Simons <simons@cryp.to>";
      author = "Peter Simons, Alexander Altman, Ben James";
      homepage = "https://github.com/peti/hackage-db#readme";
      url = "";
      synopsis = "Access cabal-install's Hackage database via Data.Map";
      description = "This library provides convenient access to the local copy of the Hackage\ndatabase that \"cabal update\" creates. Check out\nhttps://github.com/peti/hackage-db/tree/master/example/ for a collection of\nsimple example programs that demonstrate how to use this code.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
          (hsPkgs."Cabal" or (builtins.throw "The Haskell package set does not contain the package: Cabal (build dependency)"))
          (hsPkgs."containers" or (builtins.throw "The Haskell package set does not contain the package: containers (build dependency)"))
          (hsPkgs."aeson" or (builtins.throw "The Haskell package set does not contain the package: aeson (build dependency)"))
          (hsPkgs."bytestring" or (builtins.throw "The Haskell package set does not contain the package: bytestring (build dependency)"))
          (hsPkgs."directory" or (builtins.throw "The Haskell package set does not contain the package: directory (build dependency)"))
          (hsPkgs."filepath" or (builtins.throw "The Haskell package set does not contain the package: filepath (build dependency)"))
          (hsPkgs."tar" or (builtins.throw "The Haskell package set does not contain the package: tar (build dependency)"))
          (hsPkgs."time" or (builtins.throw "The Haskell package set does not contain the package: time (build dependency)"))
          (hsPkgs."utf8-string" or (builtins.throw "The Haskell package set does not contain the package: utf8-string (build dependency)"))
          ];
        };
      exes = {
        "list-known-versions" = {
          depends = (pkgs.lib).optionals (flags.install-examples) [
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."Cabal" or (builtins.throw "The Haskell package set does not contain the package: Cabal (build dependency)"))
            (hsPkgs."containers" or (builtins.throw "The Haskell package set does not contain the package: containers (build dependency)"))
            (hsPkgs."hackage-db" or (builtins.throw "The Haskell package set does not contain the package: hackage-db (build dependency)"))
            (hsPkgs."bytestring" or (builtins.throw "The Haskell package set does not contain the package: bytestring (build dependency)"))
            ];
          };
        "show-meta-data" = {
          depends = (pkgs.lib).optionals (flags.install-examples) [
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."Cabal" or (builtins.throw "The Haskell package set does not contain the package: Cabal (build dependency)"))
            (hsPkgs."containers" or (builtins.throw "The Haskell package set does not contain the package: containers (build dependency)"))
            (hsPkgs."hackage-db" or (builtins.throw "The Haskell package set does not contain the package: hackage-db (build dependency)"))
            (hsPkgs."utf8-string" or (builtins.throw "The Haskell package set does not contain the package: utf8-string (build dependency)"))
            ];
          };
        "show-package-versions" = {
          depends = (pkgs.lib).optionals (flags.install-examples) [
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."Cabal" or (builtins.throw "The Haskell package set does not contain the package: Cabal (build dependency)"))
            (hsPkgs."containers" or (builtins.throw "The Haskell package set does not contain the package: containers (build dependency)"))
            (hsPkgs."hackage-db" or (builtins.throw "The Haskell package set does not contain the package: hackage-db (build dependency)"))
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/ElvishJerricco/hackage-db.git";
      rev = "84ca9fc75ad45a71880e938e0d93ea4bde05f5bd";
      sha256 = "0y3kw1hrxhsqmyx59sxba8npj4ya8dpgjljc21gkgdvdy9628q4c";
      });
    }
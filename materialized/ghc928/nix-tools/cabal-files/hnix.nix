{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = { optimize = true; profiling = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "hnix"; version = "0.16.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "johnw@newartisans.com";
      author = "John Wiegley";
      homepage = "https://github.com/haskell-nix/hnix#readme";
      url = "";
      synopsis = "Haskell implementation of the Nix language";
      description = "Haskell implementation of the Nix language.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."deriving-compat" or (errorHandler.buildDepError "deriving-compat"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."gitrev" or (errorHandler.buildDepError "gitrev"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hashing" or (errorHandler.buildDepError "hashing"))
          (hsPkgs."hnix-store-core" or (errorHandler.buildDepError "hnix-store-core"))
          (hsPkgs."hnix-store-remote" or (errorHandler.buildDepError "hnix-store-remote"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."lens-family" or (errorHandler.buildDepError "lens-family"))
          (hsPkgs."lens-family-core" or (errorHandler.buildDepError "lens-family-core"))
          (hsPkgs."lens-family-th" or (errorHandler.buildDepError "lens-family-th"))
          (hsPkgs."logict" or (errorHandler.buildDepError "logict"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."monadlist" or (errorHandler.buildDepError "monadlist"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."neat-interpolation" or (errorHandler.buildDepError "neat-interpolation"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."ref-tf" or (errorHandler.buildDepError "ref-tf"))
          (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
          (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."some" or (errorHandler.buildDepError "some"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-lift-instances" or (errorHandler.buildDepError "th-lift-instances"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."xml" or (errorHandler.buildDepError "xml"))
          ];
        buildable = true;
        };
      exes = {
        "hnix" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."free" or (errorHandler.buildDepError "free"))
            (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
            (hsPkgs."hnix" or (errorHandler.buildDepError "hnix"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."ref-tf" or (errorHandler.buildDepError "ref-tf"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."repline" or (errorHandler.buildDepError "repline"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = if compiler.isGhc && (compiler.version).lt "8.10"
            then false
            else true;
          };
        };
      tests = {
        "hnix-tests" = {
          depends = [
            (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
            (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hnix" or (errorHandler.buildDepError "hnix"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."neat-interpolation" or (errorHandler.buildDepError "neat-interpolation"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-th" or (errorHandler.buildDepError "tasty-th"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "hnix-benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hnix" or (errorHandler.buildDepError "hnix"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hnix-0.16.0.tar.gz";
      sha256 = "d0b5a93efe6bec97b4b1af6703be6cd4935240dd1616df3ab5c006a13a374b61";
      });
    }) // {
    package-description-override = "cabal-version:  2.2\nname:           hnix\nversion:        0.16.0\nsynopsis:       Haskell implementation of the Nix language\ndescription:    Haskell implementation of the Nix language.\ncategory:       System, Data, Nix\nhomepage:       https://github.com/haskell-nix/hnix#readme\nbug-reports:    https://github.com/haskell-nix/hnix/issues\nauthor:         John Wiegley\nmaintainer:     johnw@newartisans.com\nlicense:        BSD-3-Clause\nlicense-file:   License\nbuild-type:     Simple\ndata-dir:       data/\nextra-source-files:\n  ChangeLog.md\n  ReadMe.md\n  License\n  data/nix/tests/lang/binary-data\n  data/nix/tests/lang/data\n  data/nix/tests/lang/dir1/a.nix\n  data/nix/tests/lang/dir2/a.nix\n  data/nix/tests/lang/dir2/b.nix\n  data/nix/tests/lang/dir3/a.nix\n  data/nix/tests/lang/dir3/b.nix\n  data/nix/tests/lang/dir3/c.nix\n  data/nix/tests/lang/dir4/a.nix\n  data/nix/tests/lang/dir4/c.nix\n  data/nix/tests/lang/eval-fail-abort.nix\n  data/nix/tests/lang/eval-fail-assert.nix\n  data/nix/tests/lang/eval-fail-bad-antiquote-1.nix\n  data/nix/tests/lang/eval-fail-bad-antiquote-2.nix\n  data/nix/tests/lang/eval-fail-bad-antiquote-3.nix\n  data/nix/tests/lang/eval-fail-blackhole.nix\n  data/nix/tests/lang/eval-fail-deepseq.nix\n  data/nix/tests/lang/eval-fail-hashfile-missing.nix\n  data/nix/tests/lang/eval-fail-missing-arg.nix\n  data/nix/tests/lang/eval-fail-path-slash.nix\n  data/nix/tests/lang/eval-fail-remove.nix\n  data/nix/tests/lang/eval-fail-scope-5.nix\n  data/nix/tests/lang/eval-fail-seq.nix\n  data/nix/tests/lang/eval-fail-substring.nix\n  data/nix/tests/lang/eval-fail-to-path.nix\n  data/nix/tests/lang/eval-fail-undeclared-arg.nix\n  data/nix/tests/lang/eval-okay-any-all.exp\n  data/nix/tests/lang/eval-okay-any-all.nix\n  data/nix/tests/lang/eval-okay-arithmetic.exp\n  data/nix/tests/lang/eval-okay-arithmetic.nix\n  data/nix/tests/lang/eval-okay-attrnames.exp\n  data/nix/tests/lang/eval-okay-attrnames.nix\n  data/nix/tests/lang/eval-okay-attrs2.exp\n  data/nix/tests/lang/eval-okay-attrs2.nix\n  data/nix/tests/lang/eval-okay-attrs3.exp\n  data/nix/tests/lang/eval-okay-attrs3.nix\n  data/nix/tests/lang/eval-okay-attrs4.exp\n  data/nix/tests/lang/eval-okay-attrs4.nix\n  data/nix/tests/lang/eval-okay-attrs5.exp\n  data/nix/tests/lang/eval-okay-attrs5.nix\n  data/nix/tests/lang/eval-okay-attrs.exp\n  data/nix/tests/lang/eval-okay-attrs.nix\n  data/nix/tests/lang/eval-okay-autoargs.exp\n  data/nix/tests/lang/eval-okay-autoargs.flags\n  data/nix/tests/lang/eval-okay-autoargs.nix\n  data/nix/tests/lang/eval-okay-backslash-newline-1.exp\n  data/nix/tests/lang/eval-okay-backslash-newline-1.nix\n  data/nix/tests/lang/eval-okay-backslash-newline-2.exp\n  data/nix/tests/lang/eval-okay-backslash-newline-2.nix\n  data/nix/tests/lang/eval-okay-builtins-add.exp\n  data/nix/tests/lang/eval-okay-builtins-add.nix\n  data/nix/tests/lang/eval-okay-builtins.exp\n  data/nix/tests/lang/eval-okay-builtins.nix\n  data/nix/tests/lang/eval-okay-callable-attrs.exp\n  data/nix/tests/lang/eval-okay-callable-attrs.nix\n  data/nix/tests/lang/eval-okay-catattrs.exp\n  data/nix/tests/lang/eval-okay-catattrs.nix\n  data/nix/tests/lang/eval-okay-closure.exp.xml\n  data/nix/tests/lang/eval-okay-closure.nix\n  data/nix/tests/lang/eval-okay-comments.exp\n  data/nix/tests/lang/eval-okay-comments.nix\n  data/nix/tests/lang/eval-okay-concat.exp\n  data/nix/tests/lang/eval-okay-concatmap.exp\n  data/nix/tests/lang/eval-okay-concatmap.nix\n  data/nix/tests/lang/eval-okay-concat.nix\n  data/nix/tests/lang/eval-okay-concatstringssep.exp\n  data/nix/tests/lang/eval-okay-concatstringssep.nix\n  data/nix/tests/lang/eval-okay-context.exp\n  data/nix/tests/lang/eval-okay-context-introspection.exp\n  data/nix/tests/lang/eval-okay-context-introspection.nix\n  data/nix/tests/lang/eval-okay-context.nix\n  data/nix/tests/lang/eval-okay-curpos.exp\n  data/nix/tests/lang/eval-okay-curpos.nix\n  data/nix/tests/lang/eval-okay-deepseq.exp\n  data/nix/tests/lang/eval-okay-deepseq.nix\n  data/nix/tests/lang/eval-okay-delayed-with.exp\n  data/nix/tests/lang/eval-okay-delayed-with-inherit.exp\n  data/nix/tests/lang/eval-okay-delayed-with-inherit.nix\n  data/nix/tests/lang/eval-okay-delayed-with.nix\n  data/nix/tests/lang/eval-okay-dynamic-attrs-2.exp\n  data/nix/tests/lang/eval-okay-dynamic-attrs-2.nix\n  data/nix/tests/lang/eval-okay-dynamic-attrs-bare.exp\n  data/nix/tests/lang/eval-okay-dynamic-attrs-bare.nix\n  data/nix/tests/lang/eval-okay-dynamic-attrs.exp\n  data/nix/tests/lang/eval-okay-dynamic-attrs.nix\n  data/nix/tests/lang/eval-okay-elem.exp\n  data/nix/tests/lang/eval-okay-elem.nix\n  data/nix/tests/lang/eval-okay-empty-args.exp\n  data/nix/tests/lang/eval-okay-empty-args.nix\n  data/nix/tests/lang/eval-okay-eq-derivations.exp\n  data/nix/tests/lang/eval-okay-eq-derivations.nix\n  data/nix/tests/lang/eval-okay-eq.exp.disabled\n  data/nix/tests/lang/eval-okay-eq.nix\n  data/nix/tests/lang/eval-okay-filter.exp\n  data/nix/tests/lang/eval-okay-filter.nix\n  data/nix/tests/lang/eval-okay-flatten.exp\n  data/nix/tests/lang/eval-okay-flatten.nix\n  data/nix/tests/lang/eval-okay-float.exp\n  data/nix/tests/lang/eval-okay-float.nix\n  data/nix/tests/lang/eval-okay-fromjson.exp\n  data/nix/tests/lang/eval-okay-fromjson.nix\n  data/nix/tests/lang/eval-okay-fromTOML.exp\n  data/nix/tests/lang/eval-okay-fromTOML.nix\n  data/nix/tests/lang/eval-okay-functionargs.exp.xml\n  data/nix/tests/lang/eval-okay-functionargs.nix\n  data/nix/tests/lang/eval-okay-getattrpos.exp\n  data/nix/tests/lang/eval-okay-getattrpos.nix\n  data/nix/tests/lang/eval-okay-getattrpos-undefined.exp\n  data/nix/tests/lang/eval-okay-getattrpos-undefined.nix\n  data/nix/tests/lang/eval-okay-getenv.exp\n  data/nix/tests/lang/eval-okay-getenv.nix\n  data/nix/tests/lang/eval-okay-hash.exp\n  data/nix/tests/lang/eval-okay-hashfile.exp\n  data/nix/tests/lang/eval-okay-hashfile.nix\n  data/nix/tests/lang/eval-okay-hashstring.exp\n  data/nix/tests/lang/eval-okay-hashstring.nix\n  data/nix/tests/lang/eval-okay-if.exp\n  data/nix/tests/lang/eval-okay-if.nix\n  data/nix/tests/lang/eval-okay-import.exp\n  data/nix/tests/lang/eval-okay-import.nix\n  data/nix/tests/lang/eval-okay-ind-string.exp\n  data/nix/tests/lang/eval-okay-ind-string.nix\n  data/nix/tests/lang/eval-okay-let.exp\n  data/nix/tests/lang/eval-okay-let.nix\n  data/nix/tests/lang/eval-okay-list.exp\n  data/nix/tests/lang/eval-okay-list.nix\n  data/nix/tests/lang/eval-okay-listtoattrs.exp\n  data/nix/tests/lang/eval-okay-listtoattrs.nix\n  data/nix/tests/lang/eval-okay-logic.exp\n  data/nix/tests/lang/eval-okay-logic.nix\n  data/nix/tests/lang/eval-okay-mapattrs.exp\n  data/nix/tests/lang/eval-okay-mapattrs.nix\n  data/nix/tests/lang/eval-okay-map.exp\n  data/nix/tests/lang/eval-okay-map.nix\n  data/nix/tests/lang/eval-okay-nested-with.exp\n  data/nix/tests/lang/eval-okay-nested-with.nix\n  data/nix/tests/lang/eval-okay-new-let.exp\n  data/nix/tests/lang/eval-okay-new-let.nix\n  data/nix/tests/lang/eval-okay-null-dynamic-attrs.exp\n  data/nix/tests/lang/eval-okay-null-dynamic-attrs.nix\n  data/nix/tests/lang/eval-okay-overrides.exp\n  data/nix/tests/lang/eval-okay-overrides.nix\n  data/nix/tests/lang/eval-okay-partition.exp\n  data/nix/tests/lang/eval-okay-partition.nix\n  data/nix/tests/lang/eval-okay-pathexists.exp\n  data/nix/tests/lang/eval-okay-pathexists.nix\n  data/nix/tests/lang/eval-okay-path.nix\n  data/nix/tests/lang/eval-okay-patterns.exp\n  data/nix/tests/lang/eval-okay-patterns.nix\n  data/nix/tests/lang/eval-okay-readDir.exp\n  data/nix/tests/lang/eval-okay-readDir.nix\n  data/nix/tests/lang/eval-okay-readfile.exp\n  data/nix/tests/lang/eval-okay-readfile.nix\n  data/nix/tests/lang/eval-okay-redefine-builtin.exp\n  data/nix/tests/lang/eval-okay-redefine-builtin.nix\n  data/nix/tests/lang/eval-okay-regex-match.exp\n  data/nix/tests/lang/eval-okay-regex-match.nix\n  data/nix/tests/lang/eval-okay-regex-split.exp\n  data/nix/tests/lang/eval-okay-regex-split.nix\n  data/nix/tests/lang/eval-okay-remove.exp\n  data/nix/tests/lang/eval-okay-remove.nix\n  data/nix/tests/lang/eval-okay-replacestrings.exp\n  data/nix/tests/lang/eval-okay-replacestrings.nix\n  data/nix/tests/lang/eval-okay-scope-1.exp\n  data/nix/tests/lang/eval-okay-scope-1.nix\n  data/nix/tests/lang/eval-okay-scope-2.exp\n  data/nix/tests/lang/eval-okay-scope-2.nix\n  data/nix/tests/lang/eval-okay-scope-3.exp\n  data/nix/tests/lang/eval-okay-scope-3.nix\n  data/nix/tests/lang/eval-okay-scope-4.exp\n  data/nix/tests/lang/eval-okay-scope-4.nix\n  data/nix/tests/lang/eval-okay-scope-6.exp\n  data/nix/tests/lang/eval-okay-scope-6.nix\n  data/nix/tests/lang/eval-okay-scope-7.exp\n  data/nix/tests/lang/eval-okay-scope-7.nix\n  data/nix/tests/lang/eval-okay-search-path.exp\n  data/nix/tests/lang/eval-okay-search-path.flags\n  data/nix/tests/lang/eval-okay-search-path.nix\n  data/nix/tests/lang/eval-okay-seq.exp\n  data/nix/tests/lang/eval-okay-seq.nix\n  data/nix/tests/lang/eval-okay-sort.exp\n  data/nix/tests/lang/eval-okay-sort.nix\n  data/nix/tests/lang/eval-okay-splitversion.exp\n  data/nix/tests/lang/eval-okay-splitversion.nix\n  data/nix/tests/lang/eval-okay-string.exp\n  data/nix/tests/lang/eval-okay-string.nix\n  data/nix/tests/lang/eval-okay-strings-as-attrs-names.exp\n  data/nix/tests/lang/eval-okay-strings-as-attrs-names.nix\n  data/nix/tests/lang/eval-okay-substring.exp\n  data/nix/tests/lang/eval-okay-substring.nix\n  data/nix/tests/lang/eval-okay-tail-call-1.exp-disabled\n  data/nix/tests/lang/eval-okay-tail-call-1.nix\n  data/nix/tests/lang/eval-okay-tojson.exp\n  data/nix/tests/lang/eval-okay-tojson.nix\n  data/nix/tests/lang/eval-okay-toxml2.exp\n  data/nix/tests/lang/eval-okay-toxml2.nix\n  data/nix/tests/lang/eval-okay-toxml.exp\n  data/nix/tests/lang/eval-okay-toxml.nix\n  data/nix/tests/lang/eval-okay-tryeval.exp\n  data/nix/tests/lang/eval-okay-tryeval.nix\n  data/nix/tests/lang/eval-okay-types.exp\n  data/nix/tests/lang/eval-okay-types.nix\n  data/nix/tests/lang/eval-okay-versions.exp\n  data/nix/tests/lang/eval-okay-versions.nix\n  data/nix/tests/lang/eval-okay-with.exp\n  data/nix/tests/lang/eval-okay-with.nix\n  data/nix/tests/lang/eval-okay-xml.exp.xml\n  data/nix/tests/lang/eval-okay-xml.nix\n  data/nix/tests/lang/imported2.nix\n  data/nix/tests/lang/imported.nix\n  data/nix/tests/lang/lib.nix\n  data/nix/tests/lang/parse-fail-dup-attrs-1.nix\n  data/nix/tests/lang/parse-fail-dup-attrs-2.nix\n  data/nix/tests/lang/parse-fail-dup-attrs-3.nix\n  data/nix/tests/lang/parse-fail-dup-attrs-4.nix\n  data/nix/tests/lang/parse-fail-dup-attrs-7.nix\n  data/nix/tests/lang/parse-fail-dup-formals.nix\n  data/nix/tests/lang/parse-fail-mixed-nested-attrs1.nix\n  data/nix/tests/lang/parse-fail-mixed-nested-attrs2.nix\n  data/nix/tests/lang/parse-fail-patterns-1.nix\n  data/nix/tests/lang/parse-fail-regression-20060610.nix\n  data/nix/tests/lang/parse-fail-uft8.nix\n  data/nix/tests/lang/parse-fail-undef-var-2.nix\n  data/nix/tests/lang/parse-fail-undef-var.nix\n  data/nix/tests/lang/parse-okay-1.nix\n  data/nix/tests/lang/parse-okay-crlf.nix\n  data/nix/tests/lang/parse-okay-dup-attrs-5.nix\n  data/nix/tests/lang/parse-okay-dup-attrs-6.nix\n  data/nix/tests/lang/parse-okay-mixed-nested-attrs-1.nix\n  data/nix/tests/lang/parse-okay-mixed-nested-attrs-2.nix\n  data/nix/tests/lang/parse-okay-mixed-nested-attrs-3.nix\n  data/nix/tests/lang/parse-okay-regression-20041027.nix\n  data/nix/tests/lang/parse-okay-regression-751.nix\n  data/nix/tests/lang/parse-okay-subversion.nix\n  data/nix/tests/lang/parse-okay-url.nix\n  data/nix/tests/lang/readDir/bar\n  data/nix/tests/lang/readDir/foo/git-hates-directories\n  data/nix/tests/local.mk\n  data/nixpkgs-all-packages.nix\n  data/let-comments.nix\n  data/let-comments-multiline.nix\n  data/simple-pretty.nix\n  data/simple.nix\n  data/nixpkgs-all-packages-pretty.nix\n  data/let.nix\n  tests/eval-compare/builtins.appendContext.nix\n  tests/eval-compare/builtins.eq-bottom-00.nix\n  tests/eval-compare/builtins.fetchurl-01.nix\n  tests/eval-compare/builtins.fromJSON-01.nix\n  tests/eval-compare/builtins.getContext.nix\n  tests/eval-compare/builtins.lessThan-01.nix\n  tests/eval-compare/builtins.mapAttrs-01.nix\n  tests/eval-compare/builtins.pathExists.nix\n  tests/eval-compare/builtins.replaceStrings-01.nix\n  tests/eval-compare/builtins.split-01.nix\n  tests/eval-compare/builtins.split-02.nix\n  tests/eval-compare/builtins.split-03.nix\n  tests/eval-compare/builtins.split-04.nix\n  tests/eval-compare/builtins.string.store.nix\n  tests/eval-compare/builtins.toJSON.nix\n  tests/eval-compare/current-system.nix\n  tests/eval-compare/ellipsis.nix\n  tests/eval-compare/ind-string-01.nix\n  tests/eval-compare/ind-string-02.nix\n  tests/eval-compare/ind-string-03.nix\n  tests/eval-compare/ind-string-04.nix\n  tests/eval-compare/ind-string-05.nix\n  tests/eval-compare/ind-string-06.nix\n  tests/eval-compare/ind-string-07.nix\n  tests/eval-compare/ind-string-08.nix\n  tests/eval-compare/ind-string-09.nix\n  tests/eval-compare/ind-string-10.nix\n  tests/eval-compare/ind-string-11.nix\n  tests/eval-compare/ind-string-12.nix\n  tests/eval-compare/ind-string-13.nix\n  tests/eval-compare/ind-string-14.nix\n  tests/eval-compare/ind-string-15.nix\n  tests/eval-compare/ind-string-16.nix\n  tests/eval-compare/ind-string-17.nix\n  tests/eval-compare/paths-01.nix\n  tests/eval-compare/placeholder.nix\n  tests/files/attrs.nix\n  tests/files/callLibs.nix\n  tests/files/eighty.nix\n  tests/files/file.nix\n  tests/files/file2.nix\n  tests/files/findFile.nix\n  tests/files/force.nix\n  tests/files/goodbye.nix\n  tests/files/hello.nix\n  tests/files/hello2.nix\n  tests/files/if-then.nix\n  tests/files/lint.nix\n  tests/files/loop.nix\n  tests/files/test.nix\n  tests/files/with.nix\n\nsource-repository head\n  type: git\n  location: https://github.com/haskell-nix/hnix\n\nflag optimize\n  description: Enable all optimization flags\n  manual: True\n  default: True\n\nflag profiling\n  description: Enable profiling\n  manual: True\n  default: False\n\nlibrary\n  exposed-modules:\n    Nix\n    Nix.Prelude\n    Nix.Utils\n    Nix.Atoms\n    Nix.Builtins\n    Nix.Cache\n    Nix.Cited\n    Nix.Cited.Basic\n    Nix.Context\n    Nix.Convert\n    Nix.Effects\n    Nix.Effects.Basic\n    Nix.Effects.Derivation\n    Nix.Eval\n    Nix.Exec\n    Nix.Expr\n    Nix.Expr.Shorthands\n    Nix.Expr.Strings\n    Nix.Expr.Types\n    Nix.Expr.Types.Annotated\n    Nix.Frames\n    Nix.Fresh\n    Nix.Fresh.Basic\n    Nix.Json\n    Nix.Lint\n    Nix.Normal\n    Nix.Options\n    Nix.Options.Parser\n    Nix.Parser\n    Nix.Pretty\n    Nix.Reduce\n    Nix.Render\n    Nix.Render.Frame\n    Nix.Scope\n    Nix.Standard\n    Nix.String\n    Nix.String.Coerce\n    Nix.TH\n    Nix.Thunk\n    Nix.Thunk.Basic\n    Nix.Type.Assumption\n    Nix.Type.Env\n    Nix.Type.Infer\n    Nix.Type.Type\n    Nix.Utils.Fix1\n    Nix.Value\n    Nix.Value.Equal\n    Nix.Value.Monad\n    Nix.Var\n    Nix.XML\n  other-modules:\n    Paths_hnix\n    Nix.Unused\n  autogen-modules:\n    Paths_hnix\n  hs-source-dirs:\n    src\n  ghc-options:\n    -Wall\n    -fprint-potential-instances\n  build-depends:\n      aeson >= 1.4.2 && < 1.6 || >= 2.0 && < 2.1\n    , array >= 0.4 && < 0.6\n    , base >= 4.12 && < 4.16\n    , base16-bytestring >= 0.1.1 && < 1.1\n    , binary >= 0.8.5 && < 0.9\n    , bytestring >= 0.10.8 && < 0.12\n    , cryptonite\n    , comonad >= 5.0.4 && < 5.1\n    , containers >= 0.5.11.0 && < 0.7\n    , data-fix >= 0.3.0 && < 0.4\n    , deepseq >= 1.4.3 && <1.5\n    , deriving-compat >= 0.3 && < 0.7\n    , directory >= 1.3.1 && < 1.4\n    , exceptions >= 0.10.0 && < 0.11\n    , filepath >= 1.4.2 && < 1.5\n    , free >= 5.1 && < 5.2\n    , gitrev >= 1.1.0 && < 1.4\n    , hashable >= 1.2.5 && < 1.5\n    , hashing >= 0.1.0 && < 0.2\n    , hnix-store-core >= 0.5.0 && < 0.6\n    , hnix-store-remote >= 0.5.0 && < 0.6\n    , http-client >= 0.5.14 && < 0.6 || >= 0.6.4 && < 0.8\n    , http-client-tls >= 0.3.5 && < 0.4\n    , http-types >= 0.12.2 && < 0.13\n    , lens-family >= 1.2.2 && < 2.2\n    , lens-family-core >= 1.2.2 && < 2.2\n    , lens-family-th >= 0.5.0 && < 0.6\n    , logict >= 0.6.0 && < 0.7 || >= 0.7.0.2 && < 0.8\n    , megaparsec >= 7.0 && < 9.3\n    , monad-control >= 1.0.2 && < 1.1\n    , monadlist >= 0.0.2 && < 0.1\n    , mtl >= 2.2.2 && < 2.3\n    , neat-interpolation >= 0.4 && < 0.6\n    , optparse-applicative >= 0.14.3 && < 0.17\n    , parser-combinators >= 1.0.1 && < 1.4\n    , pretty-show >= 1.9.5 && < 1.11\n    , prettyprinter >= 1.7.0 && < 1.8\n    , process >= 1.6.3 && < 1.7\n    , ref-tf >= 0.5 && < 0.6\n    , regex-tdfa >= 1.2.3 && < 1.4\n    , relude >= 1.0.0 && < 1.1.0\n    , scientific >= 0.3.6 && < 0.4\n    , semialign >= 1.2 && < 1.3\n    , serialise >= 0.2.1 && < 0.3\n    , some >= 1.0.1 && < 1.1\n    , split >= 0.2.3 && < 0.3\n    , syb >= 0.7 && < 0.8\n    , template-haskell >= 2.13 && < 2.18\n    -- provides:\n    --   * compat instances for old versions of TH for old GHCs\n    --   * orphan instances for TH missing instances\n    -- aka Lift Text, Bytestring, Vector, Containers,\n    -- we use Lift Text particulrarly for GHC 8.6\n    , th-lift-instances >= 0.1 && < 0.2\n    , text >= 1.2.3 && < 1.3\n    , these >= 1.0.1 && < 1.2\n    , time >= 1.8.0 && < 1.9 || >= 1.9.3 && < 1.10\n    , transformers >= 0.5.5 && < 0.6\n    , transformers-base >= 0.4.5 && < 0.5\n    , unix-compat >= 0.4.3 && < 0.6\n    , unordered-containers >= 0.2.9 && < 0.3\n    , vector >= 0.12.0 && < 0.13\n    , xml >= 1.3.14 && < 1.4\n  default-extensions:\n      NoImplicitPrelude\n    , OverloadedStrings\n    , DeriveGeneric\n    , DeriveDataTypeable\n    , DeriveFunctor\n    , DeriveFoldable\n    , DeriveTraversable\n    , DeriveLift\n    , FlexibleContexts\n    , FlexibleInstances\n    , ScopedTypeVariables\n    , StandaloneDeriving\n    , TypeApplications\n    , TypeSynonymInstances\n    , InstanceSigs\n    , MultiParamTypeClasses\n    , TupleSections\n    , LambdaCase\n    , BangPatterns\n    , ViewPatterns\n  if flag(optimize)\n    default-extensions:\n      ApplicativeDo\n    ghc-options:\n      -O2\n      -fexpose-all-unfoldings\n      -fspecialise-aggressively\n  -- if !flag(profiling)\n  --   build-depends:\n  --       ghc-datasize\n  default-language: Haskell2010\n\nexecutable hnix\n  hs-source-dirs:\n    main\n  main-is: Main.hs\n  other-modules:\n    Repl\n    Paths_hnix\n  autogen-modules:\n    Paths_hnix\n  ghc-options:\n    -Wall\n  build-depends:\n      aeson\n    , base\n    , comonad\n    , containers\n    , data-fix\n    , deepseq\n    , exceptions\n    , filepath\n    , free\n    , haskeline >= 0.8.0.0 && < 0.9\n    , hnix\n    , optparse-applicative\n    , pretty-show\n    , prettyprinter\n    , ref-tf\n    , relude\n    , repline >= 0.4.0.0 && < 0.5\n    , serialise\n    , template-haskell\n    , time\n  default-extensions:\n      NoImplicitPrelude\n    , OverloadedStrings\n    , DeriveGeneric\n    , DeriveDataTypeable\n    , DeriveFunctor\n    , DeriveFoldable\n    , DeriveTraversable\n    , DeriveLift\n    , FlexibleContexts\n    , FlexibleInstances\n    , ScopedTypeVariables\n    , StandaloneDeriving\n    , TypeApplications\n    , TypeSynonymInstances\n    , InstanceSigs\n    , MultiParamTypeClasses\n    , TupleSections\n    , LambdaCase\n    , BangPatterns\n    , ViewPatterns\n  if flag(optimize)\n    default-extensions:\n      ApplicativeDo\n    ghc-options:\n      -O2\n      -fexpose-all-unfoldings\n      -fspecialise-aggressively\n  if impl(ghc < 8.10)\n    -- GHC < 8.10 comes with haskeline < 0.8, which we don't support.\n    -- To simplify CI, we just disable the component.\n    buildable: False\n  default-language: Haskell2010\n\ntest-suite hnix-tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  other-modules:\n    EvalTests\n    NixLanguageTests\n    ParserTests\n    PrettyParseTests\n    PrettyTests\n    ReduceExprTests\n    TestCommon\n  hs-source-dirs:\n    tests\n  ghc-options:\n    -Wall\n    -threaded\n  build-depends:\n      Diff\n    , Glob\n    , base\n    , containers\n    , data-fix\n    , directory\n    , exceptions\n    , filepath\n    , hedgehog\n    , hnix\n    , megaparsec\n    , neat-interpolation\n    , optparse-applicative\n    , pretty-show\n    , prettyprinter\n    , process\n    , relude\n    , split\n    , tasty\n    , tasty-hedgehog\n    , tasty-hunit\n    , tasty-th\n    , serialise\n    , template-haskell\n    , time\n    , unix-compat\n  default-extensions:\n      NoImplicitPrelude\n    , OverloadedStrings\n    , DeriveGeneric\n    , DeriveDataTypeable\n    , DeriveFunctor\n    , DeriveFoldable\n    , DeriveTraversable\n    , DeriveLift\n    , FlexibleContexts\n    , FlexibleInstances\n    , ScopedTypeVariables\n    , StandaloneDeriving\n    , TypeApplications\n    , TypeSynonymInstances\n    , InstanceSigs\n    , MultiParamTypeClasses\n    , TupleSections\n    , LambdaCase\n    , BangPatterns\n    , ViewPatterns\n  if flag(optimize)\n    default-extensions:\n      ApplicativeDo\n    ghc-options:\n      -O2\n      -fexpose-all-unfoldings\n      -fspecialise-aggressively\n  default-language: Haskell2010\n\nbenchmark hnix-benchmarks\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  other-modules:\n    ParserBench\n  hs-source-dirs:\n    benchmarks\n  ghc-options:\n    -Wall\n  build-depends:\n      base\n    , criterion\n    , data-fix\n    , exceptions\n    , filepath\n    , hnix\n    , optparse-applicative\n    , relude\n    , serialise\n    , template-haskell\n    , time\n  default-extensions:\n      NoImplicitPrelude\n    , OverloadedStrings\n    , DeriveGeneric\n    , DeriveDataTypeable\n    , DeriveFunctor\n    , DeriveFoldable\n    , DeriveTraversable\n    , DeriveLift\n    , FlexibleContexts\n    , FlexibleInstances\n    , ScopedTypeVariables\n    , StandaloneDeriving\n    , TypeApplications\n    , TypeSynonymInstances\n    , InstanceSigs\n    , MultiParamTypeClasses\n    , TupleSections\n    , LambdaCase\n    , BangPatterns\n    , ViewPatterns\n  if flag(optimize)\n    default-extensions:\n      ApplicativeDo\n    ghc-options:\n      -O2\n      -fexpose-all-unfoldings\n      -fspecialise-aggressively\n  default-language: Haskell2010\n";
    }
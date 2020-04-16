let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "iserv-proxy"; version = "8.8.2"; };
      license = "BSD-3-Clause";
      copyright = "XXX";
      maintainer = "XXX";
      author = "XXX";
      homepage = "";
      url = "";
      synopsis = "iserv allows GHC to delegate Tempalte Haskell computations";
      description = "GHC can be provided with a path to the iserv binary with\n@-pgmi=/path/to/iserv-bin@, and will in combination with\n@-fexternal-interpreter@, compile Template Haskell though the\n@iserv-bin@ delegate. This is very similar to how ghcjs has been\ncompiling Template Haskell, by spawning a separate delegate (so\ncalled runner on the javascript vm) and evaluating the splices\nthere.\n\niserv can also be used in combination with cross compilation. For\nthis, the @iserv-proxy@ needs to be built on the host, targeting the\nhost (as it is running on the host). @cabal install -flibrary\n-fproxy@ will yield the proxy.\n\nUsing the cabal for the target @arch-platform-target-cabal install\n-flibrary@ will build the required library that contains the ffi\n@startSlave@ function, which needs to be invoked on the target\n(e.g. in an iOS application) to start the remote iserv slave.\n\ncalling the GHC cross compiler with @-fexternal-interpreter\n-pgmi=\$HOME/.cabal/bin/iserv-proxy -opti\\<ip address\\> -opti\\<port\\>@\nwill cause it to compile Template Haskell via the remote at \\<ip address\\>.\n\nThus to get cross compilation with Template Haskell follow the\nfollowing receipt:\n\n* compile the iserv library for your target\n\n> iserv \$ arch-platform-target-cabal install -flibrary\n\n* setup an application for your target that calls the\n* startSlave function. This could be either haskell or your\n* targets ffi capable language, if needed.\n\n>  void startSlave(false /* verbose */, 5000 /* port */,\n>                  \"/path/to/storagelocation/on/target\");\n\n* build the iserv-proxy\n\n> iserv \$ cabal install -flibrary -fproxy\n* Start your iserv-slave app on your target running on say @10.0.0.1:5000@\n* compiler your sources with -fexternal-interpreter and the proxy\n\n> project \$ arch-platform-target-ghc ModuleContainingTH.hs \\\n>             -fexternal-interpreter \\\n>             -pgmi=\$HOME/.cabal/bin/iserv-proxy \\\n>             -opti10.0.0.1 -opti5000\n\nShould something not work as expected, provide @-opti-v@ for verbose\nlogging of the @iserv-proxy@.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      exes = {
        "iserv-proxy" = {
          depends = [
            (hsPkgs."array" or (buildDepError "array"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."ghci" or (buildDepError "ghci"))
            (hsPkgs."libiserv" or (buildDepError "libiserv"))
            ];
          buildable = true;
          hsSourceDirs = [ "src" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../utils/iserv-proxy; }
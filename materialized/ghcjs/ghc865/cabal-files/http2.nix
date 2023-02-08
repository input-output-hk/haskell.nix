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
    flags = { devel = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "http2"; version = "1.6.5"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "https://github.com/kazu-yamamoto/http2";
      url = "";
      synopsis = "HTTP/2 library including frames, priority queues and HPACK";
      description = "HTTP/2 library including frames, priority queues and HPACK.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."network-byte-order" or (errorHandler.buildDepError "network-byte-order"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          ];
        buildable = true;
        };
      exes = {
        "hpack-encode" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."http2" or (errorHandler.buildDepError "http2"))
            (hsPkgs."network-byte-order" or (errorHandler.buildDepError "network-byte-order"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            ];
          buildable = if flags.devel then true else false;
          };
        "hpack-debug" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."http2" or (errorHandler.buildDepError "http2"))
            (hsPkgs."network-byte-order" or (errorHandler.buildDepError "network-byte-order"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            ];
          buildable = if flags.devel then true else false;
          };
        "hpack-stat" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."http2" or (errorHandler.buildDepError "http2"))
            (hsPkgs."network-byte-order" or (errorHandler.buildDepError "network-byte-order"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            ];
          buildable = if flags.devel then true else false;
          };
        "frame-encode" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."http2" or (errorHandler.buildDepError "http2"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = if flags.devel then true else false;
          };
        };
      tests = {
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = true;
          };
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."network-byte-order" or (errorHandler.buildDepError "network-byte-order"))
            (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            ];
          buildable = true;
          };
        "hpack" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."network-byte-order" or (errorHandler.buildDepError "network-byte-order"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            ];
          buildable = true;
          };
        "frame" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."http2" or (errorHandler.buildDepError "http2"))
            (hsPkgs."network-byte-order" or (errorHandler.buildDepError "network-byte-order"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "priority" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."heaps" or (errorHandler.buildDepError "heaps"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."network-byte-order" or (errorHandler.buildDepError "network-byte-order"))
            (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            ];
          buildable = true;
          };
        "header-compression" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."network-byte-order" or (errorHandler.buildDepError "network-byte-order"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/http2-1.6.5.tar.gz";
      sha256 = "16ebd1ee766f7a62a2cd2bd6ff98d68dbef85d512a847c8515b49a6b37f295ee";
      });
    }) // {
    package-description-override = "Name:                   http2\nVersion:                1.6.5\nx-revision: 1\nAuthor:                 Kazu Yamamoto <kazu@iij.ad.jp>\nMaintainer:             Kazu Yamamoto <kazu@iij.ad.jp>\nLicense:                BSD3\nLicense-File:           LICENSE\nSynopsis:               HTTP/2 library including frames, priority queues and HPACK\nDescription:            HTTP/2 library including frames, priority queues and HPACK.\nHomepage:               https://github.com/kazu-yamamoto/http2\nCategory:               Network\nCabal-Version:          >= 1.10\nBuild-Type:             Simple\nExtra-Source-Files:     ChangeLog.md\n                        test-hpack/hpack-test-case/go-hpack/*.json\n                        test-hpack/hpack-test-case/haskell-http2-linear/*.json\n                        test-hpack/hpack-test-case/haskell-http2-linear-huffman/*.json\n                        test-hpack/hpack-test-case/haskell-http2-naive/*.json\n                        test-hpack/hpack-test-case/haskell-http2-naive-huffman/*.json\n                        test-hpack/hpack-test-case/haskell-http2-static/*.json\n                        test-hpack/hpack-test-case/haskell-http2-static-huffman/*.json\n                        test-hpack/hpack-test-case/nghttp2/*.json\n                        test-hpack/hpack-test-case/nghttp2-16384-4096/*.json\n                        test-hpack/hpack-test-case/nghttp2-change-table-size/*.json\n                        test-hpack/hpack-test-case/node-http2-hpack/*.json\n                        test-frame/http2-frame-test-case/continuation/*.json\n                        test-frame/http2-frame-test-case/data/*.json\n                        test-frame/http2-frame-test-case/error/*.json\n                        test-frame/http2-frame-test-case/goaway/*.json\n                        test-frame/http2-frame-test-case/headers/*.json\n                        test-frame/http2-frame-test-case/ping/*.json\n                        test-frame/http2-frame-test-case/priority/*.json\n                        test-frame/http2-frame-test-case/push_promise/*.json\n                        test-frame/http2-frame-test-case/rst_stream/*.json\n                        test-frame/http2-frame-test-case/settings/*.json\n                        test-frame/http2-frame-test-case/window_update/*.json\n                        bench-hpack/headers.hs\n\n----------------------------------------------------------------\n\nSource-Repository head\n  Type:                 git\n  Location:             git://github.com/kazu-yamamoto/http2\n\nFlag devel\n  Description:          Development commands\n  Default:              False\n\n----------------------------------------------------------------\n\nLibrary\n  Default-Language:     Haskell2010\n  GHC-Options:          -Wall\n  Exposed-Modules:      Network.HPACK\n                        Network.HPACK.Table\n                        Network.HPACK.Token\n                        Network.HTTP2\n                        Network.HTTP2.Priority\n  Other-Modules:        Network.HPACK.Buffer\n                        Network.HPACK.Builder\n                        Network.HPACK.Huffman\n                        Network.HPACK.Huffman.Bit\n                        Network.HPACK.Huffman.ByteString\n                        Network.HPACK.Huffman.Decode\n                        Network.HPACK.Huffman.Encode\n                        Network.HPACK.Huffman.Params\n                        Network.HPACK.Huffman.Table\n                        Network.HPACK.Huffman.Tree\n                        Network.HPACK.HeaderBlock\n                        Network.HPACK.HeaderBlock.Decode\n                        Network.HPACK.HeaderBlock.Encode\n                        Network.HPACK.HeaderBlock.Integer\n                        Network.HPACK.Table.Dynamic\n                        Network.HPACK.Table.Entry\n                        Network.HPACK.Table.RevIndex\n                        Network.HPACK.Table.Static\n                        Network.HPACK.Types\n                        Network.HTTP2.Decode\n                        Network.HTTP2.Encode\n                        Network.HTTP2.Priority.PSQ\n                        Network.HTTP2.Priority.Queue\n                        Network.HTTP2.Types\n  Build-Depends:        base >= 4.8 && < 5\n                      , array\n                      , bytestring >= 0.10 && < 0.11\n                      , case-insensitive\n                      , containers >= 0.5\n                      , network-byte-order\n                      , psqueues\n                      , stm\n\n----------------------------------------------------------------\n\nTest-Suite doctest\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  HS-Source-Dirs:       test\n  Ghc-Options:          -Wall\n  Main-Is:              doctests.hs\n  Build-Depends:        base >= 4 && < 5\n                      , doctest >= 0.9.3\n\nTest-Suite spec\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  HS-Source-Dirs:       test, .\n  Ghc-Options:          -Wall\n  Main-Is:              Spec.hs\n  Other-Modules:        HPACK.DecodeSpec\n                        HPACK.EncodeSpec\n                        HPACK.HeaderBlock\n                        HPACK.HuffmanSpec\n                        HPACK.IntegerSpec\n                        HTTP2.FrameSpec\n                        HTTP2.PrioritySpec\n                        Network.HPACK\n                        Network.HPACK.Buffer\n                        Network.HPACK.Builder\n                        Network.HPACK.HeaderBlock\n                        Network.HPACK.HeaderBlock.Decode\n                        Network.HPACK.HeaderBlock.Encode\n                        Network.HPACK.HeaderBlock.Integer\n                        Network.HPACK.Huffman\n                        Network.HPACK.Huffman.Bit\n                        Network.HPACK.Huffman.Decode\n                        Network.HPACK.Huffman.Encode\n                        Network.HPACK.Huffman.Params\n                        Network.HPACK.Huffman.Table\n                        Network.HPACK.Huffman.Tree\n                        Network.HPACK.Table\n                        Network.HPACK.Table.Dynamic\n                        Network.HPACK.Table.Entry\n                        Network.HPACK.Table.RevIndex\n                        Network.HPACK.Table.Static\n                        Network.HPACK.Token\n                        Network.HPACK.Types\n                        Network.HTTP2.Decode\n                        Network.HTTP2.Encode\n                        Network.HTTP2.Priority\n                        Network.HTTP2.Priority.PSQ\n                        Network.HTTP2.Priority.Queue\n                        Network.HTTP2.Types\n  Build-Depends:        base >= 4 && < 5\n                      , array\n                      , base16-bytestring\n                      , bytestring < 0.11\n                      , case-insensitive\n                      , containers\n                      , hspec >= 1.3\n                      , network-byte-order\n                      , psqueues\n                      , stm\n                      , word8\n\nTest-Suite hpack\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  HS-Source-Dirs:       test-hpack, .\n  Ghc-Options:          -Wall\n  Main-Is:              Spec.hs\n  Other-Modules:        HPACKDecode\n                        HPACKSpec\n                        JSON\n                        Network.HPACK\n                        Network.HPACK.Buffer\n                        Network.HPACK.Builder\n                        Network.HPACK.HeaderBlock\n                        Network.HPACK.HeaderBlock.Decode\n                        Network.HPACK.HeaderBlock.Encode\n                        Network.HPACK.HeaderBlock.Integer\n                        Network.HPACK.Huffman\n                        Network.HPACK.Huffman.Bit\n                        Network.HPACK.Huffman.Decode\n                        Network.HPACK.Huffman.Encode\n                        Network.HPACK.Huffman.Params\n                        Network.HPACK.Huffman.Table\n                        Network.HPACK.Huffman.Tree\n                        Network.HPACK.Table\n                        Network.HPACK.Table.Dynamic\n                        Network.HPACK.Table.Entry\n                        Network.HPACK.Table.RevIndex\n                        Network.HPACK.Table.Static\n                        Network.HPACK.Token\n                        Network.HPACK.Types\n  Build-Depends:        base >= 4 && < 5\n                      , aeson\n                      , aeson-pretty\n                      , array\n                      , base16-bytestring\n                      , bytestring < 0.11\n                      , case-insensitive\n                      , containers\n                      , directory\n                      , filepath\n                      , hspec >= 1.3\n                      , network-byte-order\n                      , text\n                      , unordered-containers\n                      , vector\n                      , word8\n\nTest-Suite frame\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  HS-Source-Dirs:       test-frame\n  GHC-Options:          -Wall\n  Main-Is:              Spec.hs\n  Other-Modules:        Case\n                        FrameSpec\n                        JSON\n  Build-Depends:        base >= 4 && < 5\n                      , Glob >= 0.9\n                      , aeson\n                      , aeson-pretty\n                      , base16-bytestring\n                      , bytestring < 0.11\n                      , directory\n                      , filepath\n                      , hspec >= 1.3\n                      , http2\n                      , network-byte-order\n                      , text\n                      , unordered-containers\n\n----------------------------------------------------------------\n\nExecutable hpack-encode\n  Default-Language:     Haskell2010\n  HS-Source-Dirs:       test-hpack\n  GHC-Options:          -Wall\n  if flag(devel)\n    Buildable:          True\n  else\n    Buildable:          False\n  Main-Is:              hpack-encode.hs\n  Other-Modules:        HPACKEncode\n                        JSON\n  Build-Depends:        base >= 4 && < 5\n                      , aeson\n                      , aeson-pretty\n                      , array\n                      , base16-bytestring\n                      , bytestring < 0.11\n                      , case-insensitive\n                      , containers\n                      , http2\n                      , network-byte-order\n                      , text\n                      , unordered-containers\n                      , vector\n                      , word8\n\nExecutable hpack-debug\n  Default-Language:     Haskell2010\n  HS-Source-Dirs:       test-hpack\n  GHC-Options:          -Wall\n  if flag(devel)\n    Buildable:          True\n  else\n    Buildable:          False\n  Main-Is:              hpack-debug.hs\n  Other-Modules:        HPACKDecode\n                        JSON\n  Build-Depends:        base >= 4 && < 5\n                      , aeson\n                      , array\n                      , base16-bytestring\n                      , bytestring < 0.11\n                      , case-insensitive\n                      , containers\n                      , http2\n                      , network-byte-order\n                      , text\n                      , unordered-containers\n                      , vector\n                      , word8\n\nExecutable hpack-stat\n  Default-Language:     Haskell2010\n  HS-Source-Dirs:       test-hpack\n  GHC-Options:          -Wall\n  if flag(devel)\n    Buildable:          True\n  else\n    Buildable:          False\n  Main-Is:              hpack-stat.hs\n  Other-Modules:        JSON\n  Build-Depends:        base >= 4 && < 5\n                      , aeson\n                      , aeson-pretty\n                      , array\n                      , bytestring < 0.11\n                      , case-insensitive\n                      , containers\n                      , directory\n                      , filepath\n                      , http2\n                      , network-byte-order\n                      , text\n                      , unordered-containers\n                      , vector\n                      , word8\n\nExecutable frame-encode\n  Default-Language:     Haskell2010\n  HS-Source-Dirs:       test-frame\n  GHC-Options:          -Wall\n  if flag(devel)\n    Buildable:          True\n  else\n    Buildable:          False\n  Main-Is:              frame-encode.hs\n  Other-Modules:        Case\n                        JSON\n  Build-Depends:        base >= 4 && < 5\n                      , aeson\n                      , aeson-pretty\n                      , base16-bytestring\n                      , bytestring < 0.11\n                      , http2\n                      , text\n                      , unordered-containers\n\nBenchmark priority\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  Hs-Source-Dirs:       bench-priority\n  Ghc-Options:          -Wall\n  Main-Is:              Main.hs\n  Other-Modules:        BinaryHeap\n                        BinaryHeapSTM\n                        DoublyLinkedQueueIO\n                        Heap\n                        RandomSkewHeap\n                        RingOfQueues\n                        RingOfQueuesSTM\n  Build-Depends:        base\n                      , array\n                      , case-insensitive\n                      , containers\n                      , criterion\n                      , heaps\n                      , mwc-random\n                      , network-byte-order\n                      , psqueues\n                      , stm\n\nBenchmark header-compression\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  Hs-Source-Dirs:       bench-hpack\n  Ghc-Options:          -Wall\n  Main-Is:              Main.hs\n  Build-Depends:        base\n                      , array\n                      , bytestring < 0.11\n                      , case-insensitive\n                      , containers\n                      , criterion\n                      , network-byte-order\n                      , stm\n";
    }
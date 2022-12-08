{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "resolv"; version = "0.1.2.0"; };
      license = "GPL-2.0-or-later";
      copyright = "";
      maintainer = "hvr@gnu.org";
      author = "Herbert Valerio Riedel";
      homepage = "";
      url = "";
      synopsis = "Domain Name Service (DNS) lookup via the libresolv standard library routines";
      description = "This package implements an API for accessing\nthe [Domain Name Service (DNS)](https://tools.ietf.org/html/rfc1035)\nresolver service via the standard @libresolv@ system library (whose\nAPI is often available directly via the standard @libc@ C library) on\nUnix systems.\n\nThis package also includes support for decoding message record types\nas defined in the following RFCs:\n\n- [RFC 1035](https://tools.ietf.org/html/rfc1035): Domain Names - Implementation And Specification\n- [RFC 1183](https://tools.ietf.org/html/rfc1183): New DNS RR Definitions\n- [RFC 2782](https://tools.ietf.org/html/rfc2782): A DNS RR for specifying the location of services (DNS SRV)\n- [RFC 2915](https://tools.ietf.org/html/rfc2915): The Naming Authority Pointer (NAPTR) DNS Resource Record\n- [RFC 3596](https://tools.ietf.org/html/rfc3596): DNS Extensions to Support IP Version 6\n- [RFC 4034](https://tools.ietf.org/html/rfc4034): Resource Records for the DNS Security Extensions\n- [RFC 4255](https://tools.ietf.org/html/rfc4255): Using DNS to Securely Publish Secure Shell (SSH) Key Fingerprints\n- [RFC 4408](https://tools.ietf.org/html/rfc4408): Sender Policy Framework (SPF) for Authorizing Use of Domains in E-Mail, Version 1\n- [RFC 5155](https://tools.ietf.org/html/rfc5155): DNS Security (DNSSEC) Hashed Authenticated Denial of Existence\n- [RFC 6844](https://tools.ietf.org/html/rfc6844): DNS Certification Authority Authorization (CAA) Resource Record\n- [RFC 6891](https://tools.ietf.org/html/rfc6891): Extension Mechanisms for DNS (EDNS(0))\n- [RFC 7553](https://tools.ietf.org/html/rfc7553): The Uniform Resource Identifier (URI) DNS Resource Record\n\nFor Windows, the package [windns](https://hackage.haskell.org/package/windns)\nprovides a compatible subset of this package's API.";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
        buildable = true;
        };
      tests = {
        "resolv." = {
          depends = [
            (hsPkgs."resolv" or (errorHandler.buildDepError "resolv"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/resolv-0.1.2.0.tar.gz";
      sha256 = "81a2bafad484db123cf8d17a02d98bb388a127fd0f822fa022589468a0e64671";
      });
    }
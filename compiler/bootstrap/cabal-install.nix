{ fetchurl, hackage, stdenv, lib, ghc, zlib, src, version }:
let dependencies =
 [ { name = "deepseq";           version = "1.4.3.0";   }
   { name = "binary";            version = "0.8.5.1";   }
   { name = "time";              version = "1.9.1";     }
   { name = "transformers";      version = "0.5.5.0";   }
   { name = "mtl";               version = "2.2.2";     }
   { name = "text";              version = "1.2.3.0";   }
   { name = "parsec";            version = "3.1.13.0";  }
   { name = "network-uri";       version = "2.6.1.0";   }
   { name = "network";           version = "2.7.0.0";   }
   { name = "HTTP";              version = "4000.3.12"; }
   { name = "zlib";              version = "0.6.2";     }
   { name = "random";            version = "1.1";       }
   { name = "stm";               version = "2.4.5.0";   }
   { name = "hashable";          version = "1.2.7.0";   }
   { name = "async";             version = "2.2.1";     }
   { name = "base16-bytestring"; version = "0.1.1.6";   }
   { name = "base64-bytestring"; version = "1.0.0.1";   }
   { name = "cryptohash-sha256"; version = "0.11.101.0";}
   { name = "resolv";            version = "0.1.1.1";   }
   { name = "mintty";            version = "0.1.2";     }
   { name = "echo";              version = "0.1.3";     }
   { name = "edit-distance";     version = "0.2.2.1";   }
   { name = "ed25519";           version = "0.0.5.0";   }
   { name = "tar";               version = "0.5.1.0";   }
   { name = "digest";            version = "0.0.1.2";   }
   { name = "zip-archive";       version = "0.3.3";     }
   { name = "hackage-security";  version = "0.5.3.0";   }
 ]; in
let depPaths = builtins.map ({name, version}:
        fetchurl {
            url = "mirror://hackage/${name}-${version}.tar.gz";
            inherit (hackage.${name}.${version}) sha256; }) dependencies;
in stdenv.mkDerivation ({
  name = "cabal-install";

  inherit version;

  passthru = {
      inherit version;
  };

  meta = {
    platforms = lib.platforms.all;
  };

  nativeBuildInputs = [ ghc zlib ];

  inherit src;

  configurePhase = "";

  buildPhase = ''
  # We need the .git directory to make the ./bootstrap command use ../Cabal.
  mkdir .git

  cd cabal-install
  # unpack dependencies, so we can bootstrap without network.
  for dep in ${builtins.concatStringsSep " " depPaths}; do
    tar xzf $dep
  done

  # make sure we have the patched (revised) .cabal files.
  ${builtins.concatStringsSep "\n"
        (builtins.map ({name, version}:
            let revInfo = hackage.${name}.${version}.revisions.default; in
            if revInfo.revNum == null || revInfo.revNum == 0
            then ""
            else let revision = fetchurl { url = "https://hackage.haskell.org/package/${name}-${version}/revision/${toString revInfo.revNum}.cabal"; inherit (revInfo) sha256; }; in
                ''cp "${revision}" "${name}-${version}/${name}.cabal"''
        ) dependencies)
  }
  mkdir $out
  ghc-pkg init $out/packages.conf.d
  EXTRA_CONFIGURE_OPTS="" SCOPE_OF_INSTALLATION="--package-db=$out/packages.conf.d" PREFIX="$out" ./bootstrap.sh --no-doc -j
  '';

  installPhase = ''

  '';
})

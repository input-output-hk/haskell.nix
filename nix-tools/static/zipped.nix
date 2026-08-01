inputs: pkgs:

let

  makeZippedTools = { customPkgs, clearStripDebugFlags ? false }: 
    let 
      hsPkgs = (import ./project.nix inputs customPkgs).hsPkgs;
      
      maybeClearStripDebugFlags = drv: 
        if clearStripDebugFlags then 
          drv.overrideDerivation (_: { stripDebugFlags = []; })
        else 
          drv;

      getNixToolsExe = name: hsPkgs.nix-tools.components.exes.${name};
      getStrippedNixToolsExe = name: maybeClearStripDebugFlags (getNixToolsExe name);

      strippedNixToolsComponents = map getStrippedNixToolsExe [
        "cabal-name" 
        "cabal-to-nix" 
        "default-setup"
        "default-setup-ghcjs"
        "hackage-to-nix" 
        "hashes-to-nix" 
        "lts-to-nix" 
        "make-install-plan" 
        "plan-to-nix" 
        "stack-repos" 
        "stack-to-nix" 
        "truncate-index" 
      ];
    in 
      customPkgs.packaging.asZip {
        name = "${customPkgs.stdenv.hostPlatform.system}-nix-tools-static";
        drvs' = [ 
          hsPkgs.cabal-install.components.exes.cabal 
          hsPkgs.hpack.components.exes.hpack 
          hsPkgs.Cabal-syntax-json.components.exes.cabal2json 
        ] ++ strippedNixToolsComponents;
      };

  
  zippedToolsNoIfdFor = fragment-name: 
    let 
      stringifyInputs = inputs: pkgs.lib.mapAttrsToList (name: value: pkgs.lib.trace "${name}=${value}" "${value}") inputs;
      # stringifyInputs = inputs: map (x: "${x}") (builtins.attrValues inputs);

      fragment-drv = "static-nix-tools-outputs.hydraJobs.${pkgs.stdenv.hostPlatform.system}.zipped.${fragment-name}";
    in
      pkgs.runCommand "${pkgs.stdenv.hostPlatform.system}-all-nix-tools" {
        requiredSystemFeatures = [ "recursive-nix" ];
        nativeBuildInputs = 
          # [ inputs.nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}.nix pkgs.gitMinimal ]
          [ (pkgs.lib.trace pkgs.nix.version pkgs.nix) pkgs.gitMinimal ]
          ++ stringifyInputs inputs
          ++ stringifyInputs inputs.haskellNix.inputs;
      } ''
        export HOME=$(mktemp -d)
        mkdir $out
        # Deliberately not `nix --offline`: that bundles `substitute = false`, so
        # anything missing from the builder's store is compiled from source rather
        # than fetched.  On a darwin builder that meant rebuilding llvm and running
        # its 57k-test check-all suite, which hung and was killed by max-silent-time
        # after 3h (ci.zw3rk.com/build/1695375).  Substitution happens daemon-side,
        # outside the recursive-nix sandbox, so it was never a hermeticity risk.
        # The two TTLs below are what --offline was actually wanted for: don't
        # re-fetch the flake inputs, which nativeBuildInputs above already pins
        # into the store.
        cp $(nix --extra-experimental-features "flakes nix-command" \
          build --accept-flake-config --no-link --print-out-paths --no-allow-import-from-derivation \
          --option tarball-ttl 4294967295 \
          --option narinfo-cache-meta-ttl 4294967295 \
          --system ${pkgs.stdenv.hostPlatform.system} \
          ${../.}#${fragment-drv})/*.zip $out/
      '';
 

  zippedToolsForDarwin = makeZippedTools {
    customPkgs = pkgs;
    clearStripDebugFlags = true;
  };


  zippedToolsForLinux = makeZippedTools {
    customPkgs = pkgs.pkgsCross.musl64;
  };


  zippedToolsForLinuxArm64 = makeZippedTools {
    customPkgs = pkgs.pkgsCross.aarch64-multiplatform-musl;
  };


  allZippedTools = 
    pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-darwin" || pkgs.stdenv.hostPlatform.system == "aarch64-darwin") {
      "nix-tools-static" = zippedToolsForDarwin;
      "nix-tools-static-no-ifd" = zippedToolsNoIfdFor "nix-tools-static";
    } 
    // 
    pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
      "nix-tools-static" = zippedToolsForLinux;
      "nix-tools-static-arm64" = zippedToolsForLinuxArm64;

      "nix-tools-static-no-ifd" = zippedToolsNoIfdFor "nix-tools-static";
      "nix-tools-static-arm64-no-ifd" = zippedToolsNoIfdFor "nix-tools-static-arm64";
    };

in


allZippedTools

 

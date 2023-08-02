{
  inputs.nixpkgs.follows = "haskellNix/nixpkgs";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";

  outputs = { nixpkgs, haskellNix, ... }:
    let
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        # TODO switch back on when ci.iog.io has builders for aarch64-linux
        # "aarch64-linux"
        "aarch64-darwin"
      ];

      inherit (nixpkgs) lib;

      # A simple thing but hard to do without screwing up lazyness.
      # We don't want packages.x to trigger evaluation of packages.y
      forEachSystem = f:
        let
          perSystem = lib.genAttrs systems f;
        in
        lib.genAttrs
          [ "apps" "checks" "devShells" "hydraJobs" "packages" ]
          (attrName: lib.genAttrs systems (system: perSystem.${system}.${attrName} or { }))
      ;
    in
    forEachSystem (system:
      let
        pkgs = haskellNix.legacyPackages.${system};

        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc928";
        };

        mkTarball = package:
          let
            name = "${package.identifier.name}-${package.identifier.version}";
            paths = builtins.attrValues package.components.exes;
          in
          pkgs.runCommand name
            { preferLocalBuild = true; }
            ''
              mkdir -p ${name}
              cp --verbose --target-directory ${name} \
                ${pkgs.lib.concatMapStringsSep "  \\\n  " (p: "${p}/bin/*") paths}

              mkdir -p $out
              tar cvzf $out/${name}.tar.gz ${name}

              mkdir -p $out/nix-support
              echo "file binary-dist $out/${name}.tar.gz" >> $out/nix-support/hydra-build-products
              # Propagate the release name of the source tarball.  This is
              # to get nice package names in channels.
              echo "${name}" >> $out/nix-support/hydra-release-name
            '';
      in
      builtins.foldl' lib.recursiveUpdate { } [
        {
          inherit (project.flake') "checks" "devShells" "hydraJobs";

          packages =
            lib.mapAttrs'
              (n: v: { name = v.exeName; value = v; })
              project.flake'.packages;
        }
        (lib.optionalAttrs (system == "x86_64-linux")
          {
            hydraJobs.binary-tarball =
              mkTarball project.projectCross.musl64.hsPkgs.nix-tools;
          })
        (lib.optionalAttrs (system == "aarch64-linux")
          {
            hydraJobs.binary-tarball =
              mkTarball project.projectCross.aarch64-multiplatform-musl.hsPkgs.nix-tools;
          })
        ({
          checks.truncate-index =
            let
              hash = "0z2jc4fibfxz88pfgjq3wk5j3v7sn34xkwb8h60hbwfwhhy63vx6";
              index-state = "2020-01-10T00:00:00Z";
            in
            pkgs.runCommand "nix-tools-test-truncate-index"
              {
                outputHashAlgo = "sha256";
                outputHash = hash;
                buildInputs = [ pkgs.wget ];
              } ''
              wget http://hackage.haskell.org/01-index.tar.gz
              ${project.hsPkgs.nix-tools.components.exes.truncate-index}/bin/truncate-index -o $out -i 01-index.tar.gz -s ${index-state}
            '';
        })
      ]);

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = "true";
  };
}

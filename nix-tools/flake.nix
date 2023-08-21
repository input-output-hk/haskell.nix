{
  inputs.nixpkgs.follows = "haskellNix/nixpkgs";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";

  outputs = { self, nixpkgs, haskellNix, ... }:
    let
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        # TODO switch back on when ci.iog.io has builders for aarch64-linux
        # "aarch64-linux"
        "aarch64-darwin"
      ];

      inherit (nixpkgs) lib;

      # keep it simple (from https://ayats.org/blog/no-flake-utils/)
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems (system:
          f (haskellNix.legacyPackages.${system}.extend self.overlays.default));

      mkTarball = pkgs:
        let
          toolset =
            let pkgs' = pkgs.extend self.overlays.default; in
            # We need to use haskell.nix compilers here
            pkgs'.nix-tools-set { compilerSelection = lib.mkForce (p: p.haskell-nix.compiler); };

          inherit (toolset) name;
        in
        pkgs.runCommand "${name}-${pkgs.hostPlatform.config}.tar.gz"
          { preferLocalBuild = true; }
          ''
            mkdir -p ${name}/bin
            cp --verbose --target-directory ${name}/bin ${toolset}/bin/*

            mkdir -p $out
            tar cvzf $out/${name}.tar.gz ${name}

            mkdir -p $out/nix-support
            echo "file binary-dist $out/${name}.tar.gz" >> $out/nix-support/hydra-build-products
          '';
    in
    {
      # this is not per-system!
      overlays.default = import ./overlay.nix;

      legacyPackages = forAllSystems (pkgs: pkgs);

      project = forAllSystems (pkgs: pkgs.nix-tools.project);

      packages = forAllSystems (pkgs:
        lib.mapAttrs'
          (n: v: { name = v.exeName; value = v; })
          pkgs.nix-tools.project.flake'.packages);

      checks = forAllSystems (pkgs:
        pkgs.nix-tools.project.flake'.checks // {
          truncate-index = import ./tests/truncate-index.nix { inherit pkgs; };
        });

      devShells = forAllSystems (pkgs:
        { default = pkgs.nix-tools.project.shell; });

      hydraJobs = forAllSystems
        (pkgs:
          # project's hydraJobs
          pkgs.nix-tools.project.flake'.hydraJobs
          # tarballs with static builds.
          // lib.optionalAttrs (pkgs.buildPlatform.system == "x86_64-linux")
            { binary-tarball = mkTarball pkgs.pkgsCross.musl64; }
          // lib.optionalAttrs (pkgs.buildPlatform.system == "aarch64-linux")
            { binary-tarball = mkTarball pkgs.pkgsCross.aarch64-multiplatform-musl; }
        );
    };

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

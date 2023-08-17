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

      mkTarball = pkgs: cross:
        let
          # Use haskell.nix compilers here
          prj = pkgs.nix-tools.project.appendModule
            ({ lib, ... }:
              { compilerSelection = lib.mkForce (p: p.haskell-nix.compiler); });
          nix-tools = prj.projectCross.${cross}.hsPkgs.nix-tools;
          pkgId = "${nix-tools.identifier.name}-${nix-tools.identifier.version}";
          exes = builtins.attrValues nix-tools.components.exes;
        in
        pkgs.runCommand pkgId
          { preferLocalBuild = true; }
          ''
            mkdir -p ${pkgId}
            cp --verbose --target-directory ${pkgId} ${pkgs.lib.concatMapStringsSep " " (p: "${p}/bin/*") exes}

            mkdir -p $out
            tar cvzf $out/${pkgId}.tar.gz ${pkgId}

            mkdir -p $out/nix-support
            echo "file binary-dist $out/${pkgId}.tar.gz" >> $out/nix-support/hydra-build-products
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
            { binary-tarball = mkTarball pkgs "musl64"; }
          // lib.optionalAttrs (pkgs.buildPlatform.system == "aarch64-linux")
            { binary-tarball = mkTarball pkgs "aarch64-multiplatform-musl"; }
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

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
          # Maybe this could be
          #     haskellNix.legacyPackages.${system}.extend self.overlays.default
          # in terms of evaluation it is the same but we don't have to repeat
          # the haskell.nix configuration.
          f (import nixpkgs {
            inherit system;
            inherit (haskellNix) config;
            overlays = [ haskellNix.overlay self.overlays.default ];
          }));

      # This is a project overlay that adds a mkTarball function that
      # makes a tarball out of a package's exe components.
      # Maybe, rather than a function, this could be a derivation under each
      # package in hsPkgs but it find that a bit more awkard to write.
      mkTarballOverlay = final: prev: {
        mkTarball = pname:
          let
            inherit (final) pkgs;
            package = final.hsPkgs.${pname};
            pkgId = "${package.identifier.name}-${package.identifier.version}";
            exes = builtins.attrValues package.components.exes;
          in
          pkgs.runCommand pkgId
            { preferLocalBuild = true; }
            ''
              mkdir -p ${pkgId}
              cp --verbose --target-directory ${pkgId} \
                ${pkgs.lib.concatMapStringsSep "  \\\n  " (p: "${p}/bin/*") exes}

              mkdir -p $out
              tar cvzf $out/${pkgId}.tar.gz ${pkgId}

              mkdir -p $out/nix-support
              echo "file binary-dist $out/${pkgId}.tar.gz" >> $out/nix-support/hydra-build-products
              # Propagate the release name of the source tarball.  This is
              # to get nice package names in channels.
              echo "${pkgId}" >> $out/nix-support/hydra-release-name
            '';
      };
    in
    {
      # this is not per-system!
      overlays.default = import ./overlay.nix;

      # This break the loop. Our overlay evaluates with the flake provided
      # haskell-nix but haskell-nix won't re-evaluate with our nix-tools.
      legacyPackages = forAllSystems (pkgs:
        self.overlays.default pkgs pkgs);

      project = forAllSystems (pkgs:
        pkgs.nix-tools.project.extend mkTarballOverlay);

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

      hydraJobs = forAllSystems (pkgs:
        # project's hydraJobs
        pkgs.nix-tools.project.flake'.hydraJobs
        # tarballs with static builds.
        // lib.optionalAttrs (pkgs.buildPlatform.system == "x86_64-linux") {
          binary-tarball = pkgs.nix-tools.project.projectCross.musl64.mkTarball "nix-tools";
        }
        // lib.optionalAttrs (pkgs.buildPlatform.system == "aarch64-linux") {
          binary-tarball = pkgs.nix-tools.project.projectCross.aarch64-multiplatform-musl.mkTarball "nix-tools";
        });
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

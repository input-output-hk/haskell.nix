{
  description = "A flake for building Hello World";

  edition = 201909;

  outputs = { self }: {
    overlay = final: prev:
      let
        composeExtensions = f: g: self: super:
          let
            fApplied = f self super;
            super' = super // fApplied;
          in fApplied // g self super';
      in (builtins.foldl' composeExtensions (_: _: { }) (import ./overlays))
      final prev;
    config = import ./config.nix;
    sources = import ./nixpkgs;
    checks = let
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" ];
      forAllSystems = f:
        builtins.listToAttrs (map (system: {
          name = system;
          value = f system;
        }) systems);
      test = system:
        import ./test {
          nixpkgsArgs = {
            inherit (self) config;
            overlays = [ self.overlay ];
            localSystem = { inherit system; };
          };
        };
      testDrv = system:
        let
          attrsToList = attrs:
            map (name: {
              inherit name;
              value = attrs.${name};
            }) (builtins.attrNames attrs);
        in builtins.listToAttrs (builtins.concatMap ({ name, value }@orig:
          if value ? run then [{
            inherit name;
            value = value.run;
          }] else if value ? type && value.type == "derivation" then
            [ orig ]
          else
            attrsToList value) (attrsToList (test system)));
    in forAllSystems testDrv;
  };
}

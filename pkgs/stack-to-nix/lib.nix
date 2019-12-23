pkgs: with pkgs; with lib;

let
  inherit (haskell.lib) overrideCabal;
  inherit (haskellPackages) hackage2nix haskellSrc2nix hpack;

  cabal-name = import ./cabal-name { inherit pkgs; };

  cabalName = path: runCommand "cabal-name" {} ''
    ${cabal-name}/bin/cabal-name ${path} > $out
  '';

  hpackToCabal = path: runCommand "hpack.cabal" {} ''
    cd ${dirOf path}; ${hpack}/bin/hpack - < ${path} > $out
  '';

  listDirectory = path:
    map (name: "${path}/${name}") (attrNames (builtins.readDir path));

  yamlToJSON = path: runCommand "yaml.json" { nativeBuildInputs = [ ruby ]; } ''
    ruby -rjson -ryaml -e "puts YAML.load(ARGF).to_json" < ${path} > $out
  '';
in

{
  cabalPackageName = root:
    let
      children = listDirectory root;
      hpack = findFirst (hasSuffix "/package.yaml")
        (throw "no Cabal or Hpack file found: ${root}") children;
      cabal = findSingle (hasSuffix ".cabal") (hpackToCabal hpack)
        (throw "more than one Cabal file: ${root}") children;
    in
    import (cabalName cabal);

  cabalToNix = self: name: src: args: options:
    let
      expr = haskellSrc2nix {
        inherit name src;
        extraCabal2nixOptions = options;
      };
    in
    overrideCabal
      (self.callPackage expr args)
      (lib.const { inherit src; });

  callHackage = self: name: version:
    self.callPackage (hackage2nix name version);

  exportYAML = term: writeText "term.yaml" (builtins.toJSON term);

  importYAML = path: lib.importJSON (yamlToJSON path);

  mergeExtensions = extensions: foldr composeExtensions (_: _: {}) extensions;
}

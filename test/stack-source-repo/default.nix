{ stackProject', recurseIntoAttrs }:

let
  project = stackProject' {
    src = ./.;
    cache = [ { is-private = false; name = "cabal-simple"; rev = "bc01ebc05a8105035c9449943046b46c8364b932"; sha256 = "003lm3pm024vhbfmii7xcdd9v2rczpflxf7gdl2pyxia7p014i8z"; subdir = "test/cabal-simple"; url = "https://github.com/input-output-hk/haskell.nix.git"; } ];
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) stack-nix;
  };
  inherit (packages.stack-source-repo.components) library;
}

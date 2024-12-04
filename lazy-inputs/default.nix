final: prev:
let
  callFlake = import prev.haskell-nix.sources.flake-compat;
in {
  haskell-nix = prev.haskell-nix // {
    sources = {
      inherit ((callFlake { pkgs = final; src = ./ghc8107; }).defaultNix) ghc8107;
      inherit ((callFlake { pkgs = final; src = ./ghc901; }).defaultNix) ghc901;
      inherit ((callFlake { pkgs = final; src = ./ghc902; }).defaultNix) ghc902;
      inherit ((callFlake { pkgs = final; src = ./ghc921; }).defaultNix) ghc921;
      inherit ((callFlake { pkgs = final; src = ./ghc922; }).defaultNix) ghc922;
      inherit ((callFlake { pkgs = final; src = ./ghc923; }).defaultNix) ghc923;
      inherit ((callFlake { pkgs = final; src = ./ghc924; }).defaultNix) ghc924;
      inherit ((callFlake { pkgs = final; src = ./ghc925; }).defaultNix) ghc925;
      inherit ((callFlake { pkgs = final; src = ./ghc926; }).defaultNix) ghc926;
      inherit ((callFlake { pkgs = final; src = ./ghc927; }).defaultNix) ghc927;
      inherit ((callFlake { pkgs = final; src = ./ghc928; }).defaultNix) ghc928;
      inherit ((callFlake { pkgs = final; src = ./ghc941; }).defaultNix) ghc941;
      inherit ((callFlake { pkgs = final; src = ./ghc942; }).defaultNix) ghc942;
      inherit ((callFlake { pkgs = final; src = ./ghc943; }).defaultNix) ghc943;
      inherit ((callFlake { pkgs = final; src = ./ghc944; }).defaultNix) ghc944;
      inherit ((callFlake { pkgs = final; src = ./ghc945; }).defaultNix) ghc945;
      inherit ((callFlake { pkgs = final; src = ./ghc946; }).defaultNix) ghc946;
      inherit ((callFlake { pkgs = final; src = ./ghc947; }).defaultNix) ghc947;
      inherit ((callFlake { pkgs = final; src = ./ghc948; }).defaultNix) ghc948;
      inherit ((callFlake { pkgs = final; src = ./ghc961; }).defaultNix) ghc961;
      inherit ((callFlake { pkgs = final; src = ./ghc962; }).defaultNix) ghc962;
      inherit ((callFlake { pkgs = final; src = ./ghc963; }).defaultNix) ghc963;
      inherit ((callFlake { pkgs = final; src = ./ghc964; }).defaultNix) ghc964;
      inherit ((callFlake { pkgs = final; src = ./ghc965; }).defaultNix) ghc965;
      inherit ((callFlake { pkgs = final; src = ./ghc966; }).defaultNix) ghc966;
      inherit ((callFlake { pkgs = final; src = ./ghc981; }).defaultNix) ghc981;
      inherit ((callFlake { pkgs = final; src = ./ghc982; }).defaultNix) ghc982;
      inherit ((callFlake { pkgs = final; src = ./ghc983; }).defaultNix) ghc983;
      inherit ((callFlake { pkgs = final; src = ./ghc984; }).defaultNix) ghc984;
      inherit ((callFlake { pkgs = final; src = ./ghc9101; }).defaultNix) ghc9101;
      inherit ((callFlake { pkgs = final; src = ./ghc912X; }).defaultNix) ghc912X;
      inherit ((callFlake { pkgs = final; src = ./ghc913; }).defaultNix) ghc913;
    } // prev.haskell-nix.sources;
  };
}

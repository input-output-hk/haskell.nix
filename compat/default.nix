hackage:
{ license-map = import ./cabal-licenses.nix; platform-map = import ./cabal-os-arch-comp.nix; host-map = import ./host-map.nix; driver = import ./driver.nix hackage; }

{ supportedSystems ? [ "x86_64-linux" ]
, ifdLevel ? 3
, checkMaterialization ? false }:
import ./release.nix { inherit supportedSystems ifdLevel checkMaterialization; }

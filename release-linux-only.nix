# This is for use on hydra machines with no access to darwin build machines
{ supportedSystems ? [ "x86_64-linux" ]
, ifdLevel ? 3
, checkMaterialization ? false }:
import ./release.nix { inherit supportedSystems ifdLevel checkMaterialization; }

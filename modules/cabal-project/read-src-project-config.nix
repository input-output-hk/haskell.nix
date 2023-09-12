{ lib, config, ... }:
with lib;
let
  readIfExists = fileName:
    # Using origSrcSubDir bypasses any cleanSourceWith.
    # `lookForCabalProject` allows us to avoid looking in source from hackage
    # for cabal.project files.  It is set in `modules/hackage-project.nix`.
    let origSrcDir = config.src.origSrcSubDir or config.src;
    in
    if (config.src.lookForCabalProject or true) &&
      builtins.elem ((__readDir origSrcDir)."${fileName}" or "") [ "regular" "symlink" ]
    then builtins.readFile (origSrcDir + "/${fileName}")
    else null;

  cabalProjectOrNull = readIfExists config.cabalProjectFileName;
  cabalProjectLocalOrNull = readIfExists "${config.cabalProjectFileName}.local";
  cabalProjectFreezeOrNull = readIfExists "${config.cabalProjectFileName}.freeze";
in
{
  _file = "haskell.nix/modules/cabal-project/read-src-project-config.nix";

  config = {
    cabalProject = lib.mkDefault cabalProjectOrNull;
    cabalProjectLocal = lib.mkDefault cabalProjectLocalOrNull;
    cabalProjectFreeze = lib.mkDefault cabalProjectFreezeOrNull;
  };
}

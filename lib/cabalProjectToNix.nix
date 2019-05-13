{ mkHackageIndex, pkgs, runCommand, nix-tools, cabal-install, ghc, hpack }:
{ hackageIndexState, src }:
let
  cabalFiles =
    builtins.filterSource (path: type:
      type == "directory" ||
      pkgs.lib.any (i: (pkgs.lib.hasSuffix i path)) [ ".project" ".cabal" "package.yaml" ])
      src;
  plan = runCommand "plan" {
    buildInputs = [ ghc hpack ];
  } ''
    tmp=$(mktemp -d)
    cd $tmp
    cp -r ${cabalFiles}/* .
    chmod +w -R .
    find . -name package.yaml -exec hpack "{}" \;
    HOME=${mkHackageIndex hackageIndexState} ${cabal-install}/bin/cabal new-configure
    HOME=$out ${nix-tools}/bin/plan-to-nix --plan-json dist-newstyle/cache/plan.json -o nix-plan
    cp -r nix-plan $out
  '';
in
  runCommand "plan-and-src" {} ''
    mkdir $out
    cp -r ${src}/* $out
    ln -sf ${plan} $out/nix-plan
  ''

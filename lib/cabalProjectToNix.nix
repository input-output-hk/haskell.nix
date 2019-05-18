{ mkHackageIndex, pkgs, runCommand, nix-tools, cabal-install, ghc, hpack, symlinkJoin }:
let defaultGhc = ghc;
    defaultCabalInstall = cabal-install;
in { hackageIndexState, src, ghc ? defaultGhc, cabal-install ? defaultCabalInstall }:
let
  cabalFiles =
    pkgs.lib.cleanSourceWith {
      inherit src;
      filter = path: type:
        type == "directory" ||
        pkgs.lib.any (i: (pkgs.lib.hasSuffix i path)) [ ".project" ".cabal" "package.yaml" ];
    };
  plan = if (builtins.compareVersions cabal-install.version "2.4.0.0") < 0
         # cabal-install versions before 2.4 will generate insufficient plan information.
         then throw "cabal-install (current version: ${cabal-install.version}) needs to be at least 2.4 for plan-to-nix to work without cabal-to-nix"
         else runCommand "plan" {
    nativeBuildInputs = [ nix-tools ghc hpack cabal-install pkgs.rsync ];
  } ''
    tmp=$(mktemp -d)
    cd $tmp
    cp -r ${cabalFiles}/* .
    chmod +w -R .
    # warning: this may not generate the proper cabal file.
    # hpack allows globbing, and turns that into module lists
    # without the source available (we cleaneSourceWith'd it),
    # this may not produce the right result.
    find . -name package.yaml -exec hpack "{}" \;
    HOME=${mkHackageIndex hackageIndexState} cabal new-configure

    export LANG=C.utf8 # Needed or stack-to-nix will die on unicode inputs
    mkdir -p $out

    # ensure we have all our .cabal files (also those generated from package.yaml) files.
    # otherwise we'd need to be careful about putting the `cabal-generator = hpack` into
    # the nix expression.  As we already called `hpack` on all `package.yaml` files we can
    # skip that step and just package the .cabal files up as well.
    #
    # This is also important as `plan-to-nix` will look for the .cabal files when generating
    # the relevant `pkgs.nix` file with the local .cabal expressions.
    rsync -a --prune-empty-dirs \
          --include '*/' --include '*.cabal' --include 'package.yaml' \
          --exclude '*' \
          $tmp/ $out/

    # make sure the path's in the plan.json are relative to $out instead of $tmp
    # this is necessary so that plan-to-nix relative path logic can work.
    substituteInPlace $tmp/dist-newstyle/cache/plan.json --replace "$tmp" "$out"

    # run `plan-to-nix` in $out.  This should produce files right there with the
    # proper relative paths.
    (cd $out && plan-to-nix --plan-json $tmp/dist-newstyle/cache/plan.json -o .)

    # move pkgs.nix to default.nix ensure we can just nix `import` the result.
    mv $out/pkgs.nix $out/default.nix
  '';
in
  runCommand "plan-and-src" { nativeBuildInputs = [ pkgs.xorg.lndir pkgs.rsync ]; } ''
    mkdir $out
    # todo: should we clean `src` to drop any .git, .nix, ... other irelevant files?
    lndir -silent "${src}" "$out"
    rsync -a ${plan}/ $out/
  ''

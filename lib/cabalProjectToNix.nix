{ dotCabal, pkgs, runCommand, nix-tools, cabal-install, ghc, hpack, symlinkJoin }:
let defaultGhc = ghc;
    defaultCabalInstall = cabal-install;
in { index-state, index-sha256 ? import ./index-state-hashes.nix index-state, src, ghc ? defaultGhc, cabal-install ? defaultCabalInstall }:
assert index-sha256 != null;
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
    nativeBuildInputs = [ nix-tools ghc hpack cabal-install pkgs.rsync pkgs.git ];
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
    HOME=${dotCabal { inherit index-state; sha256 = index-sha256; }} cabal new-configure

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
  # TODO: We really want this (symlinks) instead of copying the source over each and
  #       every time.  However this will not work with sandboxed builds.  They won't
  #       have access to `plan` or `src` paths.  So while they will see all the
  #       links, they won't be able to read any of them.
  #
  #       We should be able to fix this if we propagaed the build inputs properly.
  #       As we are `import`ing the produced nix-path here, we seem to be losing the
  #       dependencies though.
  #
  #       I guess the end-result is that ifd's don't work well with symlinks.
  #
  # symlinkJoin {
  #   name = "plan-and-src";
  #   # todo: should we clean `src` to drop any .git, .nix, ... other irelevant files?
  #   buildInputs = [ plan src ];
  # }
  runCommand "plan-and-src" { nativeBuildInputs = [ pkgs.rsync ]; } ''
    mkdir $out
    # todo: should we clean `src` to drop any .git, .nix, ... other irelevant files?
    rsync -a "${src}/" "$out/"
    rsync -a ${plan}/ $out/
  ''

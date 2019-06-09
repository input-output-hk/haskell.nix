{ dotCabal, pkgs, runCommand, nix-tools, cabal-install, ghc, hpack, symlinkJoin, cacert, index-state-hashes }:
let defaultGhc = ghc;
    defaultCabalInstall = cabal-install;
in { index-state ? null, index-sha256 ? null, src, ghc ? defaultGhc, cabal-install ? defaultCabalInstall }:

# cabal-install versions before 2.4 will generate insufficient plan information.
assert (if (builtins.compareVersions cabal-install.version "2.4.0.0") < 0
         then throw "cabal-install (current version: ${cabal-install.version}) needs to be at least 2.4 for plan-to-nix to work without cabal-to-nix"
         else true);
let
  maybeCleanedSource =
    if pkgs.lib.canCleanSource src
    then pkgs.lib.cleanSourceWith {
      src = builtins.trace "src = ${src};" src;
      filter = path: type:
        type == "directory" ||
        pkgs.lib.any (i: (pkgs.lib.hasSuffix i path)) [ ".project" ".cabal" "package.yaml" ]; }
    else src;

  # Look for a index-state: field in the cabal.project file
  index-state-found = if index-state != null then index-state
    else
      let
        rawCabalProject = builtins.readFile ((cabalFiles.origSrc or cabalFiles) + "/cabal.project");
        indexState = pkgs.lib.lists.concatLists (
          pkgs.lib.lists.filter (l: l != null)
            (builtins.map (l: builtins.match "^index-state: *(.*)" l)
              (pkgs.lib.splitString "\n" rawCabalProject)));
      in
        pkgs.lib.lists.head (indexState ++ [ null ]);

  # Lookup hash for the index state we found
  index-sha256-found = if index-sha256 != null
    then index-sha256
    else index-state-hashes.${index-state-found} or null;

in
  assert (if index-state-found == null
    then throw "No index state passed and none found in cabal.project" else true);
  assert (if index-sha256-found == null
    then throw "provided sha256 for index-state ${index-state-found} is null!" else true);

let
  plan = runCommand "plan-to-nix-pkgs" {
    nativeBuildInputs = [ nix-tools ghc hpack cabal-install pkgs.rsync pkgs.git ];
  } ''
    tmp=$(mktemp -d)
    cd $tmp
    cp -r ${maybeCleanedSource}/* .
    chmod +w -R .
    # warning: this may not generate the proper cabal file.
    # hpack allows globbing, and turns that into module lists
    # without the source available (we cleaneSourceWith'd it),
    # this may not produce the right result.
    find . -name package.yaml -exec hpack "{}" \;
    export SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt
    export GIT_SSL_CAINFO=${cacert}/etc/ssl/certs/ca-bundle.crt
    HOME=${dotCabal {
      index-state = index-state-found;
      sha256 = index-sha256-found; }} cabal new-configure \
        --with-ghc=${ghc.targetPrefix}ghc \
        --with-ghc-pkg=${ghc.targetPrefix}ghc-pkg

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
  runCommand "plan-to-nix-pkgs-with-src" { nativeBuildInputs = [ pkgs.rsync ]; } ''
    mkdir $out
    # todo: should we clean `src` to drop any .git, .nix, ... other irelevant files?
    rsync -a "${src}/" "$out/"
    rsync -a ${plan}/ $out/
    # Rsync will have made $out read only and that can cause problems when
    # nix sandboxing is enabled (since it can prevent nix from moving the directory
    # out of the chroot sandbox).
    chmod +w $out
  ''

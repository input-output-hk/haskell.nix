{ pkgs, nix, runCommand, checkMaterialization }@defaults:
{ sha256
, sha256Arg      # Name of the sha256 argument for more meaningful
                 # error messages when checking the materialization.
, materialized   # null or path where to find materialized version of
                 # the output. If this is set but does not exist
                 # the derivation will fail but with a message
                 # advising how to populate it.
, reasonNotSafe  # Some times there a reasont the derivation will
                 # not produce output that can be safely materialized.
                 # Set this to a string explaining why and materialization
                 # will not be used (if sha256 was set an error will be
                 # displayed including the reasonNotSafe string).
, checkMaterialization ? defaults.checkMaterialization
                 # When checkMaterialization is set the derivation
                 # will be calculated the slow way (without using `sha256`
                 # and `materialized`) the result will be used to check
                 # `sha256` and `materialized` (if set).
}: derivation:

let
  inherit (derivation) name;

  traceIgnoringSha256 = reason: x:
    if sha256 != null
      then builtins.trace ("Warning: ignoring sha256 for " + name + " " + reason) x
      else x;

  traceIgnoringMaterialized = reason: x:
    if materialized != null
      then builtins.trace ("Warning: ignoring materialized for " + name + " " + reason) x
      else x;

  unchecked =
    if reasonNotSafe != null
      then
        # Warn the user if they tried to pin stuff down when it is not safe
        traceIgnoringSha256 reasonNotSafe
          (traceIgnoringMaterialized reasonNotSafe calculateNoHash)
    else if sha256 == null
      then
        # Let the user know how to calculate a sha256 to use to make this
        # a fixed output derivation.
        builtins.trace ("Get `${sha256Arg}` with `nix-hash --base32 --type sha256 "
          + toString calculateNoHash + "/`") (traceIgnoringMaterialized
            "${sha256Arg} is not set" calculateNoHash)
    else if materialized == null
      then
        # To avoid any IFD dependencies add a `materialized` copy somewhere
        # and pass it in.
        builtins.trace ("To materialize, point `materialized` to a copy of " + toString calculateUseHash)
          calculateUseHash
    else
      # Everything is in place we can safely use the sha256 and materialized
      calculateUseAll;

  # Build fully and check the hash and materialized versions
  checked = runCommand name {
    buildInputs = [ nix ];
  } (''
        ERR=$(mktemp -d)/errors.txt
      ''
    + (pkgs.lib.optionalString (sha256 != null) ''
        NEW_HASH=$(nix-hash --base32 --type sha256 ${calculateNoHash})
        if [ "${sha256}" != "$NEW_HASH" ]; then
          echo Changes to ${name} not reflected in ${sha256Arg}
          diff -ru ${calculateUseHash} ${calculateNoHash} || true
          echo "Calculated hash for ${name} was not ${sha256}. New hash is :" >> $ERR
          echo "    ${sha256Arg} = \"$NEW_HASH\";"                      >> $ERR
        else
          echo ${sha256Arg} used for ${name} is correct
        fi
      '')
    + (
      if materialized != null && !__pathExists materialized
        then ''
          echo "Materialized nix used for ${name} is missing. To fix run :" >> $ERR
          echo "    cp -r ${calculateNoHash} ${toString materialized}"      >> $ERR
          echo "    chmod -R +w ${toString materialized}"                   >> $ERR
          cat $ERR
          false
        ''
        else
          (pkgs.lib.optionalString (materialized != null && __pathExists materialized) ''
            if diff -qr ${materialized} ${calculateNoHash} &>/dev/null; then
              echo materialized nix used for ${name} is correct
              else
              echo Changes to plan not reflected in materialized nix for ${name}
              diff -ru ${materialized} ${calculateNoHash} || true
              echo "Materialized nix used for ${name} incorrect. To fix run :" >> $ERR
              echo "    rm -rf ${toString materialized}"                       >> $ERR
              echo "    cp -r ${calculateNoHash} ${toString materialized}"     >> $ERR
              echo "    chmod -R +w ${toString materialized}"                  >> $ERR
            fi
          '')
        + ''
            if [ -e $ERR ]; then
              cat $ERR
              false
            else
              cp -r ${unchecked} $out
              # Make sure output files can be removed from the sandbox
              chmod -R +w $out
            fi
          '')
  );

  hashArgs = {
    outputHashMode = "recursive";
    outputHashAlgo = "sha256";
    outputHash = sha256;
  };
  calculateNoHash = derivation;
  calculateUseHash =
    # Use `cp -r` here to get rid of symlinks so we know the result
    # can be safely materialized (no symlinks to the store).
    runCommand name hashArgs ''
      cp -r ${derivation} $out
      # Make sure output files can be removed from the sandbox
      chmod -R +w $out
    '';
  calculateUseAll = 
    # Skip right to expectedPath if it already exists
    if materialized != null
      then
        assert __pathExists materialized;
        runCommand name hashArgs ''
          cp -r ${materialized} $out
          # Make sure output files can be removed from the sandbox
          chmod -R +w $out
        ''
      else calculateUseHash;

  # Materialized location was specified, but the files are not there.
  missingMaterialized = materialized != null && !__pathExists materialized;

in
  # Use the checked version if requested or if the `materialized` version
  # is missing (perhaps deleted or not created yet).
  if checkMaterialization || missingMaterialized
    then checked
    else unchecked

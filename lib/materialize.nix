{ pkgs, nix, runCommand, writeShellScript, checkMaterialization }@defaults:
{ sha256 ? null  # Has to make this a fixed output derivation
, sha256Arg ? "sha256"
                 # Name of the sha256 argument for more meaningful
                 # error messages when checking the materialization.
, materialized   # null or path where to find materialized version of
                 # the output. If this is set but does not exist
                 # the derivation will fail but with a message
                 # advising how to populate it.
, reasonNotSafe ? null
                 # Some times there a reasont the derivation will
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
    let
      sha256message = "To make this a fixed-output derivation but not materialized, set `${sha256Arg}` to the output of ${calculateMaterializedSha}";
      materializeMessage = "To materialize the output entirely, point `materialized` to a writable path and pass the path to ${updateMaterialized}";
    in if reasonNotSafe != null
      then
        # Warn the user if they tried to pin stuff down when it is not safe
        traceIgnoringSha256 reasonNotSafe
          (traceIgnoringMaterialized reasonNotSafe calculateNoHash)
    else if materialized != null
      then calculateUseMaterialized
    else if sha256 != null
      then
        # Let the user know how to materialize if they want to.
        builtins.trace materializeMessage calculateUseHash
    else # materialized == null && sha256 == null
        # Let the user know how to calculate a sha256 or materialize if they want to.
        builtins.trace sha256message (builtins.trace materializeMessage calculateNoHash);

  # Build fully and check the hash and materialized versions
  checked = runCommand name {
    buildInputs = [ nix ];
  } (''
        ERR=$(mktemp -d)/errors.txt
      ''
    + (pkgs.lib.optionalString (sha256 != null) ''
        NEW_HASH=$(${calculateMaterializedSha})
        if [[ ${sha256} != $NEW_HASH ]]; then
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
          echo "Materialized nix used for ${name} is missing. To fix run: ${updateMaterialized}" >> $ERR
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
              echo "Materialized nix used for ${name} incorrect. To fix run: ${updateMaterialized}" >> $ERR
            fi
          '')
        + ''
            if [[ -e $ERR ]]; then
              cat $ERR
              false
            else
              cp -Lr ${unchecked} $out
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
    # Use `cp -Lr` here to get rid of symlinks so we know the result
    # can be safely materialized (no symlinks to the store).
    runCommand name hashArgs ''
      cp -Lr ${derivation} $out
      # Make sure output files can be removed from the sandbox
      chmod -R +w $out
    '';
  calculateUseMaterialized =
    assert materialized != null;
    assert __pathExists materialized;
    runCommand name (pkgs.lib.optionalAttrs (sha256 == null) hashArgs) ''
      cp -Lr ${materialized} $out
      # Make sure output files can be removed from the sandbox
      chmod -R +w $out
    '';
  calculateMaterializedSha =
    writeShellScript "calculateSha" ''
      nix-hash --base32 --type sha256 ${calculateNoHash}
  '';

  updateMaterialized =
    assert materialized != null;
    writeShellScript "fixMaterialized" ''
      # The target is either the argument to the script if there is one, or else the current location of the materialized files
      TARGET=''${1:-${toString materialized}}

      # Crudely try and guard people from writing to the Nix store accidentally
      if [[ ''${TARGET##/nix/store/} != $TARGET ]]; then
         echo "Attempted to write to $TARGET in the Nix store! Put your materialized files somewhere else!"
         exit 1
      fi

      # Update the files
      rm -rf $TARGET
      cp -r ${calculateNoHash} "$TARGET"
      chmod -R +w "$TARGET"
  '';

  # Materialized location was specified, but the files are not there.
  missingMaterialized = materialized != null && !__pathExists materialized;

  # Use the checked version if requested or if the `materialized` version
  # is missing (perhaps deleted or not created yet).
  result = if checkMaterialization || missingMaterialized
    then checked
    else unchecked;

in result
   # Also include the script to fix the materialization files in passthru.
   // { passthru = (result.passthru or {}) // { inherit updateMaterialized calculateMaterializedSha; }; }

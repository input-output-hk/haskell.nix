{ indexStateHashesPath, nix-tools, coreutils, nix, writeShellScriptBin, stdenv, curl }:
with builtins;
with stdenv.lib;
writeShellScriptBin "update-index-state-hashes" ''
   export PATH="${makeBinPath [ coreutils nix-tools nix curl ]}"
   
   # We'll take the last element from the indexStatesHashes file via nix and get the name.
   # This is the last timestamp recorded in the file (implicit assumption: the file is
   # ordered, and nix preserved that order when parsing it into a attributeset).
   start=${let ls = attrNames (import indexStateHashesPath); in elemAt ls (length ls - 1)}
   
   # The indexStatesHashesPath looks like
   # {
   #   ...
   # }
   # Idea: take everything but drop the last line, and can then just append each new
   # entry and finally close the file with "}".  We'll do this by echoing to STDOUT!
   
   # Old file without the closing curly brace.
   cat ${indexStateHashesPath} | head -n -1
   
   # Parse the $start date, and now into seconds with the date command.  Then walk
   # them by 86400 (24*60*60) days.  We need to format the output with '%.f' as we 
   # don't want fractional values.
   for d in $(seq -f '%.f' $(date -u +%s -d $start) 86400 $(date -u +%s)) ; do
      # turn the step date $d into a YYYY-MM-DD string, and generate the truncated
      # index, compute it's hash and echo "$dT00:00:00Z" = "$sha256"; to STDOUT.
      dt=$(date -u +%Y-%m-%d -d @$d)

      # ensure we don't generate the $start date twice (skip the first invocation).
      if [[ "''${dt}T00:00:00Z" != "$start" ]]; then
        curl "https://hackage.haskell.org/01-index.tar.gz" --output index.tar.gz --fail-early
        truncate-index -o ''${dt}-01-index.tar.gz -i index.tar.gz -s "''${dt}T00:00:00Z"
        sha256=$(nix-hash --flat --type sha256 ''${dt}-01-index.tar.gz)
        echo "  \"''${dt}T00:00:00Z\" = \"''${sha256}\";"
      fi
    done
    
    # emit the final closing brace.
    echo '}'
    ''

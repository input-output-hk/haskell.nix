{ indexStateHashesPath, nix-tools, coreutils, nix, writeShellScriptBin, stdenv }:
with builtins;
with stdenv.lib;
writeShellScriptBin "update-index-state-hashes" ''
   export PATH="${getBin coreutils}/bin:${getBin nix-tools}/bin:${getBin nix}/bin:$PATH"
   start=${let ls = attrNames (import indexStateHashesPath); in elemAt ls (length ls - 1)}
   cat ${indexStateHashesPath} | head -n -1
   for d in $(seq -f '%.f' $(date -u +%s -d $start) 86400 $(date -u +%s)) ; do
      dt=$(date -u +%Y-%m-%d -d @$d)
      if [[ "''${dt}T00:00:00Z" != "$start" ]]; then
        truncate-index -o ''${dt}-01-index.tar.gz -i ${fetchurl "https://hackage.haskell.org/01-index.tar.gz"} -s "''${dt}T00:00:00Z"
        sha256=$(nix-hash --flat --type sha256 ''${dt}-01-index.tar.gz)
        echo "  \"''${dt}T00:00:00Z\" = \"''${sha256}\";"
      fi
    done
    echo '}'
    ''

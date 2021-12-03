#!/usr/bin/env bash
for ghc in $(find .  -maxdepth 1 -mindepth 1 -type d | sort); do
  (cd $ghc
   echo "{" > default.nix
   (for a in $(find . -name "*.patch" -type f | sort); do
     b=${a%%.patch};
     b=${b##./};
     if [[ "$ghc" =~ js- ]]; then
       echo "  packages.$b.patches = [ ({ version, revision }: $a) ];" >> default.nix
     else
       echo "  packages.${b%%-*}.patches = [ ({ version, revision }: if version == \"${b##*-}\" && revision == 0 then $a else null) ];" >> default.nix
     fi
   done) || true
   echo "}" >> default.nix)
done

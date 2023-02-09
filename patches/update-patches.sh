#!/usr/bin/env bash
for ghc in $(find . -name "ghc*" -type d | sort); do
  (cd $ghc
   echo "{" > default.nix
   (for a in $(find . -name "*.patch" -type f | sort); do
     b=${a%%.patch};
     b=${b##./};
     echo "  packages.${b%%-*}.patches = [ ({ version }: if version == \"${b##*-}\" then $a else null) ];" >> default.nix
   done) || true
   echo "}" >> default.nix)
done

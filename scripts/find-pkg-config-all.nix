# Script used to help make the lists of pkc-config packages in lib/pkgconf-nipkgs-map.nix.
# It searches for .pc files in every package it can find.
#
# This really is not a great way to do this and if we have to run it again
# it might be worht making some improvements or finding something else to do it.
#
# I had hoped the `-j0` would stop it from building anything (but it does not) and
# setting `--builders ""` did not seem to help either.  Instead I ran multiple copies
# in multiple terminals like this:
#
#   $(nix-build scripts/find-pkg-config-all.nix --argstr startAt "v") > v.txt
#
# Hitting ctrl+c if one of them started building anything massive (causes
# nix-build to fail and the script moves on to the next one).
#
# I monitored the progress with:
#   tail -f v.txt
# (in even more teminals)
#
# If one of them caught up to the next one I killed it.
#
{ startAt ? "" }:
let
  pkgs = import <nixpkgs> { config = { allowUnfree = true; allowAliases = false; }; };
in
  pkgs.writeScript "find-pkg-config-all" ''
    ${ pkgs.lib.concatStrings (map (name: pkgs.lib.optionalString (name >= startAt && name != "AAAAAASomeThingsFailToEvaluate" && null == builtins.match ".*[^0-9A-Za-z_-].*" name) ''
        OUT_DIR=$(mktemp -d)
        if nix-build -E 'let pkgs = import <nixpkgs> {}; in pkgs.lib.getDev pkgs."${name}"' -j0 --out-link $OUT_DIR/result >&2; then
          find $OUT_DIR/*/ -name '*.pc' | sed 's|^.*/\([^/]*\)\.pc|    "\1" = [ "${name}" ];|' | sort -u
          rm $OUT_DIR/result*
        fi
      ''
      ) (__attrNames pkgs))
    }

    echo '# pkgs.xorg'
    ${ pkgs.lib.concatStrings (map (name: pkgs.lib.optionalString (null == builtins.match ".*[^0-9A-Za-z_-].*" name) ''
        OUT_DIR=$(mktemp -d)
        if nix-build -E 'let pkgs = import <nixpkgs> {}; in pkgs.lib.getDev pkgs.xorg."${name}"' -j0 --out-link $OUT_DIR/result >&2; then
          find $OUT_DIR/*/ -name '*.pc' | sed 's|^.*/\([^/]*\)\.pc|    "\1" = [ "${name}" ];|' | sort -u
          rm $OUT_DIR/result*
        fi
      ''
      ) (__attrNames pkgs.xorg))
    }
  ''

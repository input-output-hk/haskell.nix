# This file is for configuration that should be required by every
# package set. Hopefully we can keep configuration for particular
# package sets out of this repo. Ideally, this file is only used for
# fixing things that are broken due to the Nix infrastructure.

{ pkgs, ... }: {
  # terminfo doesn't list libtinfo in its cabal file. We could ignore
  # this if we used the terminfo shipped with GHC, but this package is
  # reinstallable so we'd rather have it defined in the plan.
  packages.terminfo.components.library.libs = [pkgs.ncurses];

  # The `extra-libraries` field in `X11.cabal` does not include Xss and Xinerama
  # see https://github.com/input-output-hk/haskell.nix/pull/988
  packages.X11.components.library.libs = [
    pkgs.xorg.libXScrnSaver
    pkgs.xorg.libXinerama
  ];
}

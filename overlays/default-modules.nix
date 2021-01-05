# This overlay adds `defaultModules` to work around issues with
# libraries in hackage.  If the work around is local to a single
# package consider using `hackage-quirks.nix` instead.
#
final: prev:
let
  inherit (final) lib;

in { haskell-nix = prev.haskell-nix // {

  defaultModules = prev.haskell-nix.defaultModules ++ [
    ({pkgs, ...}: {
      # The `extra-libraries` field in `X11.cabal` does not include Xss and Xinerama
      # see https://github.com/input-output-hk/haskell.nix/pull/988
      packages.X11.components.library.libs = [
        pkgs.xorg.libXScrnSaver
        pkgs.xorg.libXinerama.dev
      ];
    })
  ];

}; }

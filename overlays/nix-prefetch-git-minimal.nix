final: prev: {
  # gitMinimal still ships with perl (breaks for windows cross compilation)
  gitReallyMinimal = (
    final.git.override {
      perlSupport = false;
      pythonSupport = false;
      withManual = false;
      withpcre2 = false;
    }
  ).overrideAttrs (
    _: {
      # installCheck is broken when perl is disabled
      doInstallCheck = false;
    }
  );

  # This can reduce closure size of nix-tools:
  #  * Eliminates dependency on python3 (70MB)
  #  * Allows sharing with `fetchgit` as it also uses `gitMinimal` (50MB)
  inherit (
    let f = import (final.path + "/pkgs/tools/package-management/nix-prefetch-scripts");
    in
      if (builtins.functionArgs f) ? git
        then final.callPackages f { gitMinimal = final.gitMinimal; }
        else prev) nix-prefetch-git;

  # fetchgit use `buildPackages.gitMinimal` and on nixpkgs 21.11
  # and earlier that causes problems when cross compiling.
  # Adding an extra `buildPackages` works around this.
  # To check for the issue run the following in `nixpkgs`:
  #   nix-diff $(nix-instantiate -A gitMinimal) $(nix-instantiate -A pkgsCross.mingwW64.buildPackages.gitMinimal)
  # These two derivations should really be the same and
  # on nixpkgs-unstable and 22.05 they are now, but it looks
  # like that might be because the dependency on libredirect
  # was changed.
  # See https://github.com/NixOS/nixpkgs/pull/182143
  libredirect = prev.libredirect.overrideAttrs (_attrs: {
    libName = "libredirect" + final.stdenv.hostPlatform.extensions.sharedLibrary;
  });

  # Find uses of the non minimal git package by uncommenting this:
  # git = prev.intentional-error-here;
  # gitMinimal = final.gitAndTools.git.override {
  #   withManual = false;
  #   pythonSupport = false;
  #   withpcre2 = false;
  # };
}

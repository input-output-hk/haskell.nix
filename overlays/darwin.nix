_final: prev:
{
  haskell-nix = prev.haskell-nix // ({
    defaultModules = prev.haskell-nix.defaultModules ++ [
      ({ pkgs, buildModules, config, lib, ... }: prev.haskell-nix.haskellLib.addPackageKeys
        {
          packages = { } // pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.isDarwin && !pkgs.stdenv.cc.nativeLibc)
            {
              # Workaround for broken nixpkgs darwin.security_tool in
              # Mojave. This mirrors the workaround in nixpkgs
              # haskellPackages.
              #
              # ref:
              # https://github.com/NixOS/nixpkgs/pull/47676
              # https://github.com/NixOS/nixpkgs/issues/45042
              x509-system.components.library.preBuild = "substituteInPlace System/X509/MacOS.hs --replace security /usr/bin/security";
            } // pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.isDarwin
              && builtins.compareVersions config.compiler.version "9.4" < 0)
            {
              # Fix broken C++ library selection on darwin for GHC < 9.4.
              # For GHC >= 9.4 the `system-cxx-std-lib` pseudo-package
              # handles c++abi linkage automatically.  Older GHCs don't
              # have that pseudo-package, so patch double-conversion's
              # cabal file to ask for c++abi explicitly.  Gated to <9.4
              # so newer GHCs (where the v2 builder runs and doesn't
              # honour `preConfigure` anyway) don't trip over this.
              double-conversion.components.library.preConfigure = ''
                substituteInPlace double-conversion.cabal --replace 'extra-libraries: c++' 'extra-libraries: c++ c++abi'
              '';
            };
        })
    ];
  });
}

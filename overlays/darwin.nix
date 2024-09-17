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
            } // pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.isDarwin)
            {
              # fix broken c++ library selection for GHC < 9.4
              # for GHC >= 9.4 the system-cxx-std-lib pseudo-package does this.
              double-conversion.components.library.preConfigure = ''
                substituteInPlace double-conversion.cabal --replace 'extra-libraries: c++' 'extra-libraries: c++ c++abi'
              '';
            };
        })
    ];
  });
}

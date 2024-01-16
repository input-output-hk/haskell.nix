{ stdenv, testSrc, haskell-nix, compiler-nix-name, evalPackages, recurseIntoAttrs, buildPackages }:
let
  project = haskell-nix.cabalProject' {
    inherit compiler-nix-name evalPackages;
    name = "haskell-language-server";
    src = haskell-nix.sources."hls-2.6";
    # Even though this is in the cabal.project it is inside a condional
    # and so haskell.nix cannot parse it properly.  Luckily adding it
    # again seems to work fine.
    cabalProjectLocal = ''
      repository head.hackage.ghc.haskell.org
        url: https://ghc.gitlab.haskell.org/head.hackage/
        secure: True
        key-threshold: 3
        root-keys:
           f76d08be13e9a61a377a85e2fb63f4c5435d40f8feb3e12eb05905edb8cdea89
           26021a13b401500c8eb2761ca95c61f2d625bfef951b939a8124ed12ecf07329
           7541f32a4ccca4f97aea3b22f5e593ba2c0267546016b992dfadcd2fe944e55d
        --sha256: sha256-Bkn2Etb0JVmb7tM7jxuIoYLFnSp7acqraEYVq0I5oUM=

      if impl(ghc < 9.7)
        active-repositories: hackage.haskell.org
    '';
    configureArgs = "--disable-benchmarks --disable-tests";
  };
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  build = project.getComponent "haskell-language-server:exe:haskell-language-server";

  # hls does not need to be cross compiled.
  meta.disabled =
    stdenv.hostPlatform != stdenv.buildPlatform
    || __compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.0.1" < 0
    || __compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.8.0" >= 0;
}

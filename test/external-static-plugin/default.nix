{ cabalProject', testSrc, compiler-nix-name, buildPackages, evalPackages, recurseIntoAttrs, haskellLib }: let
  project = cabalProject' {
    src = testSrc "external-static-plugin";
    inherit compiler-nix-name evalPackages;
    modules = [ {
      packages.prog.components.exes.prog.plugins = [ {
        inherit (project.hsPkgs.plugin.components) library;
        moduleName = "Plugin";
        args = [ "f1" "f2" ];
      } ];
      packages.prog.postInstall = ''
        test -f f1
        test -f f2
      '';
    } ];
  };
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  meta.disabled =
    __compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.6" < 0
      || haskellLib.isCrossHost;

  build = project.hsPkgs.prog.components.exes.prog;
}

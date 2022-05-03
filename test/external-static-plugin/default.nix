{ cabalProject', testSrc, compiler-nix-name, recurseIntoAttrs, haskellLib }: let
  project = cabalProject' {
    src = testSrc "external-static-plugin";
    inherit compiler-nix-name;
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

  meta.disabled = !(builtins.elem compiler-nix-name [
    "ghc810420210212"
  ]) || haskellLib.isCrossHost;

  build = project.hsPkgs.prog.components.exes.prog;
}

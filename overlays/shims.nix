# Shims to make older versions of nixpkgs work
final: prev: final.optionalAttrs (!(prev ? runCommandLocal)) {
  # nixpkgs 19.09 and older does not have runCommandLocal
  runCommandLocal = name: env:
    final.runCommand name (env // {
      preferLocalBuild = true;
      allowSubstitutes = false;
    });
}
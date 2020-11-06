# Shims to make older versions of nixpkgs work
final: prev: {
  # nixpkgs 19.09 and older does not have runCommandLocal
  runCommandLocal = prev.runCommandLocal or (name: env:
    final.runCommand name (env // {
      preferLocalBuild = true;
      allowSubstitutes = false;
    }));
}
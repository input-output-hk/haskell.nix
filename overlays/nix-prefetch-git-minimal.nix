final: prev: {
  # This can reduce closure size of nix-tools:
  #  * Eliminates dependency on python3 (70MB)
  #  * Allows sharing with `fetchgit` as it also uses `gitMinimal` (50MB)
  inherit (final.callPackages (final.path + "/pkgs/tools/package-management/nix-prefetch-scripts") {
    git = final.gitMinimal;
  }) nix-prefetch-git;
}

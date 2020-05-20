final: prev: {
  # This overlay sets `evalPackages` as just `buildPackages`
  # See `eval-on-current.nix` for alternative that uses
  # `builtins.currentSystem`.
  evalPackages = final.buildPackages;
}
let
  sources = import (../nix/sources.nix) {};
  nixpkgs = import sources.nixpkgs-2009 {};
  args = '' --arg userDefaults "$HOME/.config/hix/hix.conf" --arg src ./.'';
  # Use HIX_ROOT to override the version of hix used when developing new hix features.
  # See docs/dev/hix.md for details.
  hixProject = "\${HIX_ROOT:-${./..}}/hix/project.nix";
in nixpkgs.symlinkJoin {
  name = "hix";
  paths = [
    (nixpkgs.writeScriptBin "hix-shell" ''
      nix-shell ${hixProject} ${args} -A shell "$@"
    '')
    (nixpkgs.writeScriptBin "hix-build" ''
      nix-build ${hixProject} ${args} "$@"
    '')
    (nixpkgs.writeScriptBin "hix-instantiate" ''
      nix-instantiate ${hixProject} ${args} "$@"
    '')
    (nixpkgs.writeScriptBin "hix-env" ''
      nix-env -f ${hixProject} ${args} "$@"
    '')
    (nixpkgs.writeScriptBin "hix" ''
      cmd=$1
      shift
      case $cmd in
      update)
        nix-env -iA hix -f https://github.com/input-output-hk/haskell.nix/tarball/master
        ;;
      build|dump-path|eval|log|path-info|run|search|show-derivation|sign-paths|verify|why-depends)
        nix $cmd -f ${hixProject} ${args} "$@"
        ;;
      repl)
        nix $cmd ${hixProject} ${args} "$@"
        ;;
      *)
        nix $cmd "$@"
        ;;
      esac
    '')
  ];
} // {
  project = import ./project.nix; 
}
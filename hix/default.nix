{ pkgs }:
let
  args = '' --arg userDefaults "$HOME/.config/hix/hix.conf" --arg src ./.'';
  # Use HIX_ROOT to override the version of hix used when developing new hix features.
  # See docs/dev/hix.md for details.
  hixProject = "\${HIX_ROOT:-${./..}}/hix/project.nix";
in pkgs.symlinkJoin {
  name = "hix";
  paths = [
    (pkgs.writeScriptBin "hix-shell" ''
      nix-shell ${hixProject} ${args} -A shell "$@"
    '')
    (pkgs.writeScriptBin "hix-build" ''
      nix-build ${hixProject} ${args} "$@"
    '')
    (pkgs.writeScriptBin "hix-instantiate" ''
      nix-instantiate ${hixProject} ${args} "$@"
    '')
    (pkgs.writeScriptBin "hix-env" ''
      nix-env -f ${hixProject} ${args} "$@"
    '')
    (pkgs.writeScriptBin "hix" ''
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

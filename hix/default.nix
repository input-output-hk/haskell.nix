{ pkgs }:
let
  args = '' --arg userDefaults "$HOME/.config/hix/hix.conf" --arg src ./.'';
  # Use HIX_ROOT to override the version of hix used when developing new hix features.
  # See docs/dev/hix.md for details.
  hixProject = "\${HIX_ROOT:-${./..}}/hix/project";
  hixInit = "\${HIX_ROOT:-${./..}}/hix/init";
  hix-shell = pkgs.writeScriptBin "hix-shell" ''
    nix-shell ${hixProject} ${args} -A shell "$@"
  '';
  hix-build = pkgs.writeScriptBin "hix-build" ''
    nix-build ${hixProject} ${args} "$@"
  '';
  hix-instantiate = pkgs.writeScriptBin "hix-instantiate" ''
    nix-instantiate ${hixProject} ${args} "$@"
  '';
  hix-env = pkgs.writeScriptBin "hix-env" ''
    nix-env -f ${hixProject} ${args} "$@"
  '';
  hix = pkgs.writeScriptBin "hix" ''
      cmd=$1
      shift
      case $cmd in
      update)
        nix-env -iA hix -f https://github.com/input-output-hk/haskell.nix/tarball/master
        ;;
      dump-path|eval|log|path-info|search|show-derivation|sign-paths|verify|why-depends)
        nix $cmd -f ${hixProject} ${args} "$@"
        ;;
      flake|build|develop|run)
        while(($#)); do
          arg=$1
          case $arg in
            --out-link|-o|--eval-store|--include|-I|--inputs-from|--expr|--file|-f|--keep|-k|--phase|--profile|--unset|-u)
              args+=("$arg" "$2")
              shift
              ;;
            --arg|--argstr|--override-flake|--override-input)
              args+=("$arg" "$2" "$3")
              shift
              shift
              ;;
            -*)
              args+=("$arg")
              ;;
            github:*#*)
              SRC="''${arg%%#*}"
              args+=("path:${hixProject}#''${arg#*#}" --override-input src "$SRC")
              is_github=true
              ;;
            github:*)
              SRC="$arg"
              args+=("path:${hixProject}" --override-input src "$SRC")
              is_github=true
              ;;
            *#*)
              SRC="''${arg%%#*}"
              args+=("path:$SRC/.hix-flake#''${arg#*#}" --override-input src "$SRC")
              has_input_src=true
              ;;
            .*)
              SRC="$arg"
              args+=("path:$SRC/.hix-flake" --override-input src "$SRC")
              has_input_src=true
              ;;
            *)
              args+=("$arg")
              ;;
          esac
          shift
        done
        if [ "$is_github" != true ]; then
          if [ "$has_input_src" != true ]; then
            SRC=.
            args+=("path:$SRC/.hix-flake" --override-input src "$SRC")
          fi
          # Make a temporary flake if we have not already
          if [ ! -d $SRC/.hix-flake ]; then
            mkdir $SRC/.hix-flake
          fi
          HIX_FLAKE="$(mktemp -d)/flake.nix"
          sed 's|EVAL_SYSTEM|${pkgs.stdenv.hostPlatform.system}|' < ${hixProject}/flake.nix > $HIX_FLAKE
          if ! cmp $HIX_FLAKE $SRC/.hix-flake/flake.nix &>/dev/null; then
            if [ -e $SRC/.hix-flake/flake.lock ]; then
              echo "Updating $SRC/.hix-flake/flake.nix and deleting old $SRC/.hix-flake/flake.lock"
              rm $SRC/.hix-flake/flake.lock
            else
              echo "Updating $SRC/.hix-flake/flake.nix" 
            fi
            cp $HIX_FLAKE $SRC/.hix-flake/flake.nix
            chmod +w $SRC/.hix-flake/flake.nix
          fi
        fi
        nix $cmd "''${args[@]}"
        ;;
      init|init-hix)
        if [ "$cmd" == "init" ]; then
          FLAKE_NIX="$(mktemp -d)/flake.nix"
          sed 's|EVAL_SYSTEM|${pkgs.stdenv.hostPlatform.system}|' < ${hixInit}/flake.nix > $FLAKE_NIX
          if [ -e flake.nix ]; then
            if ! diff -u flake.nix $FLAKE_NIX; then
              echo 'ERROR: Not replacing existing `flake.nix`.'
              exit 1
            fi
          else
            cp $FLAKE_NIX flake.nix
            echo '`flake.nix` file created.'
          fi
        fi
        HIX_NIX="$(mktemp -d)/hix.nix"
        sed 's|EVAL_SYSTEM|${pkgs.stdenv.hostPlatform.system}|' < ${hixInit}/nix/hix.nix > $HIX_NIX
        if [ -e nix/hix.nix ]; then
          echo '`nix/hix.nix` project configuration already exists:'
        else
          mkdir -p nix
          cp $HIX_NIX nix/hix.nix
          echo '`nix/hix.nix` project configuation:'
        fi
        cat nix/hix.nix
        ;;
      repl)
        nix $cmd ${hixProject} ${args} "$@"
        ;;
      *)
        nix $cmd "$@"
        ;;
      esac
    '';
in (pkgs.symlinkJoin {
  name = "hix";
  paths = [ hix-build hix-shell hix-instantiate hix-env hix ];
}) // {
  apps = { inherit hix-build hix-shell hix-instantiate hix-env hix; };
}

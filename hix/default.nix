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
      #! /usr/bin/env bash
      cmd=$1
      shift
      case $cmd in
      update)
        nix-env -iA hix -f https://github.com/input-output-hk/haskell.nix/tarball/master
        ;;
      dump-path|eval|log|path-info|search|show-derivation|sign-paths|verify|why-depends)
        nix $cmd -f ${hixProject} ${args} "$@"
        ;;
      flake|build|develop|run|profile)
        # Put the flake files for remote URLs in $HOME/.hix by default
        HIX_DIR="''${HIX_DIR:-$HOME/.hix}"
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
              FLAKE=$HIX_DIR/$SRC
              args+=("path:$FLAKE#''${arg#*#}" --override-input src "$SRC")
              has_input_src=true
              ;;
            github:*)
              SRC="$arg"
              FLAKE=$HIX_DIR/$SRC
              args+=("path:$FLAKE" --override-input src "$SRC")
              has_input_src=true
              ;;
            *#*)
              SRC="''${arg%%#*}"
              FLAKE=$SRC/.hix-flake
              args+=("path:$FLAKE#''${arg#*#}" --override-input src "$SRC")
              has_input_src=true
              ;;
            .*)
              SRC="$arg"
              FLAKE=$SRC/.hix-flake
              args+=("path:$FLAKE" --override-input src "$SRC")
              has_input_src=true
              ;;
            *)
              args+=("$arg")
              ;;
          esac
          shift
        done
        if [ "$has_input_src" != true ]; then
          SRC=.
          FLAKE=$SRC/.hix-flake
          args+=("path:$FLAKE" --override-input src "$SRC")
        fi
        # Make a temporary flake if we have not already
        mkdir -p $FLAKE
        HIX_FLAKE="$(mktemp -d)/flake.nix"
        sed 's|EVAL_SYSTEM|${pkgs.stdenv.hostPlatform.system}|' < ${hixProject}/flake.nix > $HIX_FLAKE
        if ! cmp $HIX_FLAKE $FLAKE/flake.nix &>/dev/null; then
            if [ -e $FLAKE/flake.lock ]; then
            echo "Updating $FLAKE/flake.nix and deleting old $FLAKE/flake.lock"
            rm $FLAKE/flake.lock
          else
            echo "Updating $FLAKE/flake.nix"
          fi
          cp $HIX_FLAKE $FLAKE/flake.nix
          chmod +w $FLAKE/flake.nix
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
        ${pkgs.bat}/bin/bat nix/hix.nix
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

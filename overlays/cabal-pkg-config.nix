final: prev:
{
  # This is a wrapper for `cabal configure` use only.
  #
  # When creating a plan for building a project cabal first
  # runs `pkg-config --list-all` for a list of all the available
  # packages installed on the system.
  #
  # It then gets the corresponding versions by passing that list
  # on the command line to `pkg-config --modversion`.
  #
  # This gives cabal a full picture of what versions are available
  # when building the plan.
  #
  # When we run `cabal configure` in `lib/call-cabal-project-to-nix.nix`
  # we do not want to depend on every pkg-config package in `nixpkgs`
  # that could be used.  We also do not want the user to have to specify
  # every pkg-config package that their project requires.
  #
  # Instead this wrapper provides a list based on the contents
  # of `lib/pkgconf-nixpkgs-map.nix`.  To avoid depending
  # on the packages it gets the versions for `--modversions` from
  # the `.version` attribute of the derivation.
  #
  # In most cases this `.version` will be suitable, however there
  # are some packages where that is not the case.  If these cause
  # issues we should first try to fix `lib/pkgconf-nixpkgs-map.nix`
  # or the package.  If that does not work we may need a way to include
  # overrides here.
  allPkgConfigWrapper =
    let
      # Try getting the `.version` attribute or failing that look in the
      # `.name`.  Some packages like `icu` have the correct version in
      # `.name` but no `.version`.
      getVersion = p: p.version or (builtins.parseDrvName (p.name or "")).version;
      pkgconfigPkgs =
        final.lib.filterAttrs (_name: p: __length p > 0 && getVersion (__head p) != "")
          (import ../lib/pkgconf-nixpkgs-map.nix final);
    in prev.pkg-config.overrideAttrs (attrs:
      let
        # These vars moved from attrs to attrs.env in nixpkgs adc8900df1758eda56abd68f7d781d1df74fa531
        # Support both for the time being.
        targetPrefix = attrs.targetPrefix or attrs.env.targetPrefix;
        baseBinName = attrs.baseBinName or attrs.env.baseBinName;
      in {
      installPhase = attrs.installPhase + ''
        mv $out/bin/${targetPrefix}${baseBinName} \
          $out/bin/${targetPrefix}${baseBinName}-wrapped

        cat <<EOF >$out/bin/${targetPrefix}${baseBinName}
        #!${final.stdenv.shell}
        if [[ "\$1" == "--list-all" ]]; then
          OUTPUT=\$(mktemp)
          ERROR=\$(mktemp)
        cat <<EOF2
        ${final.pkgs.lib.concatStrings (map (name: ''
          ${name}
        '') (__attrNames pkgconfigPkgs))
         }EOF2
        elif [[ "\$1" == "--modversion" ]]; then
          OUTPUT=\$(mktemp)
          ERROR=\$(mktemp)
        cat <<EOF2
        ${final.pkgs.lib.concatStrings (map (p: ''
          ${getVersion (builtins.head p)}
        '') (__attrValues pkgconfigPkgs))
        }EOF2
        else
          $out/bin/${targetPrefix}${baseBinName}-wrapped "\$@"
        fi
        EOF
        chmod +x $out/bin/${targetPrefix}${baseBinName}
      '';
  });
  # cabal 3.8 asks pkg-config for linker options for both
  # dynamic and static linking.
  # For some derivations (glib for instance) pkg-config can
  # fail when `--static` is passed.  This might be because
  # the library only has dynamic libraries.
  #
  # To work around this problem this wrapper makes cabal lazy
  # by return a single command line option when it fails.
  # That option should never be used and if it is hopefully
  # the name of the option itself will be helpful.
  #
  # See https://github.com/input-output-hk/haskell.nix/issues/1642
  #
  cabalPkgConfigWrapper = prev.pkg-config.overrideAttrs (attrs: (
  let
    # These vars moved from attrs to attrs.env in nixpkgs adc8900df1758eda56abd68f7d781d1df74fa531
    # Support both for the time being.
    targetPrefix = attrs.targetPrefix or attrs.env.targetPrefix;
    baseBinName = attrs.baseBinName or attrs.env.baseBinName;
  in {
    installPhase = attrs.installPhase + ''
      mv $out/bin/${targetPrefix}${baseBinName} \
        $out/bin/${targetPrefix}${baseBinName}-wrapped

      cat <<EOF >$out/bin/${targetPrefix}${baseBinName}
      #!${final.stdenv.shell}
      if [[ "\$1" == "--libs" && "\$2" == "--static" ]]; then
        OUTPUT=\$(mktemp)
        ERROR=\$(mktemp)
        if $out/bin/${targetPrefix}${baseBinName}-wrapped "\$@" >output 2>\$ERROR; then
          cat \$OUTPUT
        else
          echo "--error-pkg-config-static-failed=\$ERROR"
        fi
      else
        $out/bin/${targetPrefix}${baseBinName}-wrapped "\$@"
      fi
      EOF
      chmod +x $out/bin/${targetPrefix}${baseBinName}
    '';
  }));
}

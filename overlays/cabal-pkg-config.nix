final: prev:
{
  # systemd's `libsystemd.pc` reports only the major version (e.g. `259`),
  # whereas the derivation `.version` is e.g. `259.3`.  Set `pc-version` so
  # `allPkgConfigWrapper` (used by plan-to-nix) reports the same value the
  # real `pkg-config` returns inside a v2 build slice, keeping the resolved
  # `libsystemd` version — and therefore the slice's UnitId — in agreement.
  # The `pkgconf-pc-version` test verifies this matches `pkg-config --modversion`.
  systemd = prev.systemd.overrideAttrs (old: {
    passthru = (old.passthru or {}) // {
      pc-version = prev.lib.versions.major prev.systemd.version;
    };
  });

  # FreeType's `freetype2.pc` advertises a libtool/ABI version (e.g. `26.2.20`)
  # that is deliberately decoupled from the release `.version` (e.g. `2.13.3`):
  # see FreeType's `docs/VERSIONS.TXT`.  `pkg-config --modversion freetype2`
  # returns the libtool number, so plan-to-nix must report the same value or
  # the v2 slice's UnitId (which folds the resolved `freetype2` version into
  # `pkgHashPkgConfigDeps`) forks — taking every transitive consumer
  # (gi-freetype2 -> gi-harfbuzz -> gi-pango -> gi-gdk3 -> gi-gtk3 -> ...) with
  # it.  The mapping is an arbitrary per-release table (no formula), so we
  # encode the known rows and fall back to `.version` for unknown releases.
  # Adding `passthru.pc-version` does not change freetype's derivation, so this
  # neither rebuilds freetype nor forces it to build during planning.
  # The `pkgconf-pc-version` test verifies this matches `pkg-config --modversion`.
  freetype = prev.freetype.overrideAttrs (old: {
    passthru = (old.passthru or {}) // {
      pc-version =
        let
          # release version -> libtool/`.pc` version, from FreeType's
          # `docs/VERSIONS.TXT`.  Add new rows here when bumping freetype.
          pcVersions = {
            "2.11.0" = "24.0.18";
            "2.11.1" = "24.1.18";
            "2.12.0" = "24.2.18";
            "2.12.1" = "24.3.18";
            "2.13.0" = "25.0.19";
            "2.13.1" = "26.0.20";
            "2.13.2" = "26.1.20";
            "2.13.3" = "26.2.20";
            "2.14.0" = "26.3.20";
            "2.14.1" = "26.4.20";
            "2.14.2" = "26.5.20";
            "2.14.3" = "26.6.20";
          };
        in pcVersions.${prev.freetype.version} or (prev.lib.warn ''
          haskell.nix: no freetype `pc-version` for ${prev.freetype.version}; using the
          release version, which freetype2.pc usually disagrees with (e.g. 2.13.3 -> 26.2.20).
          Add a row to overlays/cabal-pkg-config.nix (see the libtool column in
          https://gitlab.freedesktop.org/freetype/freetype/-/raw/master/docs/VERSIONS.TXT):
              "${prev.freetype.version}" = "<`pkg-config --modversion freetype2`>";
        '' prev.freetype.version);
    };
  });

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
      # Prefer an explicit `pc-version` when the derivation carries one:
      # some packages' `.pc` `Version:` field differs from the derivation
      # `.version` (e.g. systemd's libsystemd.pc reports `259` while the
      # derivation `.version` is `259.3`).  The real `pkg-config` the v2
      # build slice runs reads the `.pc` field, so plan-to-nix must report
      # the same value here or the slice's UnitId (which folds the resolved
      # pkgconfig-dep version into `pkgHashPkgConfigDeps`) will fork.
      #
      # Failing that, try the `.version` attribute, or failing that look in
      # the `.name`.  Some packages like `icu` have the correct version in
      # `.name` but no `.version`.
      getVersion = p: p.pc-version or p.version or (builtins.parseDrvName (p.name or "")).version;
      pkgconfigPkgs =
        final.lib.filterAttrs (_name: p: __length p > 0 && getVersion (__head p) != "")
          (import ../lib/pkgconf-nixpkgs-map.nix final);
    in prev.pkg-config.overrideAttrs (attrs:
      let
        # These vars moved from attrs to attrs.env in nixpkgs adc8900df1758eda56abd68f7d781d1df74fa531
        # ... and then 706de783c83f3e24e5ea2a28e1249320aa19f57e moved them to attrs.passthru
        # Support all three for the time being.
        targetPrefix = attrs.targetPrefix
                    or attrs.env.targetPrefix
                    or attrs.passthru.targetPrefix;
        baseBinName = attrs.baseBinName
                   or attrs.env.baseBinName
                   or attrs.passthru.baseBinName;
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
    # ... and then 706de783c83f3e24e5ea2a28e1249320aa19f57e moved them to attrs.passthru
    # Support all three for the time being.
    targetPrefix = attrs.targetPrefix
                or attrs.env.targetPrefix
                or attrs.passthru.targetPrefix;
    baseBinName = attrs.baseBinName
               or attrs.env.baseBinName
               or attrs.passthru.baseBinName;
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

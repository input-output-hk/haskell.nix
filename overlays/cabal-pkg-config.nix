final: prev:
{
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
  cabalPkgConfigWrapper = prev.pkgconfig.overrideAttrs (attrs: {
    installPhase = attrs.installPhase + ''
      mv $out/bin/${attrs.targetPrefix}${attrs.baseBinName} \
        $out/bin/${attrs.targetPrefix}${attrs.baseBinName}-wrapped

      cat <<EOF >$out/bin/${attrs.targetPrefix}${attrs.baseBinName}      
      #!${final.stdenv.shell}
      if [[ "\$1" == "--libs" && "\$2" == "--static" ]]; then
        OUTPUT=\$(mktemp)
        ERROR=\$(mktemp)
        if $out/bin/${attrs.targetPrefix}${attrs.baseBinName}-wrapped "\$@" >output 2>\$ERROR; then
          cat \$OUTPUT
        else
          echo "--error-pkg-config-static-failed=\$ERROR"
        fi
      else
        $out/bin/${attrs.targetPrefix}${attrs.baseBinName}-wrapped "\$@"
      fi
      EOF
      chmod +x $out/bin/${attrs.targetPrefix}${attrs.baseBinName}
    '';
  });
}

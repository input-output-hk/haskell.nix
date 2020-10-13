# haskell-gi packages need gobject-introspection turned on
final: prev: {
  # As of nixpkgs 20.09 harfbuzz is built with mesonFlags and hopefully includes
  # gobject introspection correctly.  Older nixpkgs will have configureFlags to update.
  harfbuzz = prev.harfbuzz.overrideAttrs (attr: final.lib.optionalAttrs (attr ? configureFlags) {
    configureFlags = attr.configureFlags ++ [ "--enable-introspection=yes" "--with-gobject=yes" ];
    buildInputs = attr.buildInputs ++ [ final.pkgs.gobject-introspection ];
  });
}

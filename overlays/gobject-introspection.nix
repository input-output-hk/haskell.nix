# haskell-gi packages need gobject-introspection turned on
final: prev: {
  harfbuzz = prev.harfbuzz.overrideAttrs (attr: {
    configureFlags = attr.configureFlags ++ [ "--enable-introspection=yes" "--with-gobject=yes" ];
    buildInputs = attr.buildInputs ++ [ final.pkgs.gobject-introspection ];
  });
}

final: prev: {
  nodejs-with-lto = prev.pkgsStatic.nodejs_24.overrideAttrs (attrs: {
    LDFLAGS="-Wl,-z,stack-size=8388608";
    VARIATION="static";
    patches = attrs.patches or [] ++ [./patches/node-lto.patch];
    configureFlags = ["--enable-lto" "--fully-static"];
  });
}

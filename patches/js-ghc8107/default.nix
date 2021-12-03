{
  packages.buffer-builder.patches = [ ({ version, revision }: ./buffer-builder.patch) ];
  packages.conduit-extra.patches = [ ({ version, revision }: ./conduit-extra.patch) ];
  packages.double-conversion.patches = [ ({ version, revision }: ./double-conversion.patch) ];
  packages.network.patches = [ ({ version, revision }: ./network.patch) ];
  packages.say.patches = [ ({ version, revision }: ./say.patch) ];
}

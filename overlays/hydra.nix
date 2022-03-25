{ sources, ...}:

final: prev: {
  hydra-unstable = sources.hydra.defaultPackage.${prev.system};
}

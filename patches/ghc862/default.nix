{
  packages.binary.patches = [ ./binary-0.8.6.0.patch ];
  packages.containers.patches = [ ./containers-0.6.0.1.patch ];
  packages.default.nix.patches = [ ./default.nix ];
  packages.hpc.patches = [ ./hpc-0.6.0.3.patch ];
  packages.parsec.patches = [ ./parsec-3.1.13.0.patch ];
  packages.process.patches = [ ./process-1.6.3.0.patch ];
  packages.time.patches = [ ./time-1.8.0.2.patch ];
  packages.transformers.patches = [ ./transformers-0.5.5.0.patch ];
  packages.unix.patches = [ ./unix-2.7.2.2.patch ];
}

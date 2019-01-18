{
  packages.hpc.patches = [ ({ version, revision }: if version == "0.6.0.3" && revision == 0 then ./hpc-0.6.0.3.patch else null) ];
  packages.containers.patches = [ ({ version, revision }: if version == "0.6.0.1" && revision == 0 then ./containers-0.6.0.1.patch else null) ];
  packages.binary.patches = [ ({ version, revision }: if version == "0.8.6.0" && revision == 0 then ./binary-0.8.6.0.patch else null) ];
  packages.parsec.patches = [ ({ version, revision }: if version == "3.1.13.0" && revision == 0 then ./parsec-3.1.13.0.patch else null) ];
  packages.time.patches = [ ({ version, revision }: if version == "1.8.0.2" && revision == 0 then ./time-1.8.0.2.patch else null) ];
  packages.unix.patches = [ ({ version, revision }: if version == "2.7.2.2" && revision == 0 then ./unix-2.7.2.2.patch else null) ];
  packages.process.patches = [ ({ version, revision }: if version == "1.6.3.0" && revision == 0 then ./process-1.6.3.0.patch else null) ];
  packages.transformers.patches = [ ({ version, revision }: if version == "0.5.5.0" && revision == 0 then ./transformers-0.5.5.0.patch else null) ];
}

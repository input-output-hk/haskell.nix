{
  packages.array.patches = [ ({ version }: if version == "0.5.2.0" then ./array-0.5.2.0.patch else null) ];
  packages.deepseq.patches = [ ({ version }: if version == "1.4.3.0" then ./deepseq-1.4.3.0.patch else null) ];
  packages.hpc.patches = [ ({ version }: if version == "0.6.0.3" then ./hpc-0.6.0.3.patch else null) ];
  packages.parallel.patches = [ ({ version }: if version == "3.2.1.1" then ./parallel-3.2.1.1.patch else null) ];
  packages.parsec.patches = [ ({ version }: if version == "3.1.13.0" then ./parsec-3.1.13.0.patch else null) ];
  packages.random.patches = [ ({ version }: if version == "1.1" then ./random-1.1.patch else null) ];
  packages.time.patches = [ ({ version }: if version == "1.8.0.2" then ./time-1.8.0.2.patch else null) ];
  packages.unix.patches = [ ({ version }: if version == "2.7.2.2" then ./unix-2.7.2.2.patch else null) ];
  packages.vector.patches = [ ({ version }: if version == "0.11.0.0" then ./vector-0.11.0.0.patch else null) ];
}

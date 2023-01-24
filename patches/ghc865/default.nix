{
  packages.binary.patches = [ ({ version }: if version == "0.8.6.0" then ./binary-0.8.6.0.patch else null) ];
  packages.containers.patches = [ ({ version }: if version == "0.6.0.1" then ./containers-0.6.0.1.patch else null) ];
  packages.parsec.patches = [ ({ version }: if version == "3.1.13.0" then ./parsec-3.1.13.0.patch else null) ];
  packages.singletons.patches = [ ({ version }: if version == "2.5.1" then ./singletons-2.5.1.patch else null) ];
  packages.unix.patches = [ ({ version }: if version == "2.7.2.2" then ./unix-2.7.2.2.patch else null) ];
}

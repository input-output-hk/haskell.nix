# Use the same effective metadata in own, dependency and build-tool indexes.
{ fragment, target, copyOptions ? "", singleLine ? false }:
if fragment.cabalFile != null
then "cp${copyOptions} ${fragment.cabalFile} ${target}"
else if singleLine
then "tar -xOzf ${fragment.tarball} ${fragment.pkgName}-${fragment.pkgVersion}/${fragment.pkgName}.cabal > ${target}"
else ''
  tar -xOzf ${fragment.tarball} \
    ${fragment.pkgName}-${fragment.pkgVersion}/${fragment.pkgName}.cabal \
    > ${target}
''

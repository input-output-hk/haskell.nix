AC_INIT([The Glorious Glasgow Haskell Compilation System], [0.00], [glasgow-haskell-bugs@haskell.org], [ghc-AC_PACKAGE_VERSION])

# Set this to YES for a released version, otherwise NO
: ${RELEASE=NO}

dnl ** Make sure we do not have a bogus source directory
AC_CONFIG_SRCDIR(ghc/Main.hs)

dnl ** Output code to perform substitutions
AC_CONFIG_FILES

dnl ** Find unixy sort and find commands,
dnl ** which are needed by FP_SETUP_PROJECT_VERSION
FP_PROG_FIND
FP_PROG_SORT

dnl ** FP_SETUP_VERSION assumes to be in the source directory
pushd ${srcdir}
FP_SETUP_PROJECT_VERSION
popd

AC_OUTPUT

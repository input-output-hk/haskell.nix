m4_include([fp_check_prog.m4])
m4_include([fp_prog_find.m4])
m4_include([fp_prog_sort.m4])
m4_include([fp_setup_project_version.m4])

AC_INIT([The Glorious Glasgow Haskell Compilation System],
        [0.0.0],
		[glasgow-haskell-bugs@haskell.org],
		[ghc])

# Set this to YES for a released version, otherwise NO
: ${RELEASE=NO}

dnl ** Make sure we do not have a bogus source directory
AC_CONFIG_SRCDIR(ghc/Main.hs)

dnl ----------------------------------------------------------
dnl ** Find unixy sort and find commands,
dnl ** which are needed by FP_SETUP_PROJECT_VERSION

FP_PROG_FIND
FP_PROG_SORT

dnl ----------------------------------------------------------
pushd ${srcdir}
FP_SETUP_PROJECT_VERSION
popd

dnl ** Output code to perform substitutions
AC_CONFIG_FILES

AC_OUTPUT

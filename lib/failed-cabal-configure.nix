let
  configurationError = ./cabal-configure.out;
in
  # Trace the error output to make sure the user has a chance to see it
  # (even if the choose not to build anything from the project)
  __trace ''
    ERROR: cabal configure failed with:
    ${__readFile configurationError}
  '' { inherit configurationError; }

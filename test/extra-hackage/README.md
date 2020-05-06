# Tests for extra Hackage functionality

This directory contains two packages, `external-package-demo` and `external-package-user`, the
second one depends on the first one. Both packages were created with `cabal init`.

`external-package-demo` was uploaded to local Hackage at `localhost` and `01-index.tar.gz` from that
Hackage was downloaded to this directory. Then the index file was processed with `hackage-to-nix`,
the result is in `hackage/` directory.

The tests check that `cabalProject'` is able to construct plan with dependencies from extra Hackage
and then build the package itself.

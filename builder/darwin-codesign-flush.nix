# Fix for an aarch64-darwin write->sign code-signing coherence race
# (haskell.nix#2018).
#
# On aarch64-darwin a freshly written large Mach-O (emitted by ld64, then
# patched by remove-references-to / install_name_tool) can be signed over
# page-cache bytes that differ from what is finally on disk, producing a
# signature the kernel later rejects ("Code Signature Invalid" -> SIGKILL).
# It is intermittent and affects binaries signed *inside* the build; macOS
# `sync` is asynchronous and does not settle it, but a real flush to stable
# storage (F_FULLFSYNC) + dropping the stale mapping (F_NOCACHE) does.
#
# We return (a singleton list containing) `autoSignDarwinBinariesHook` patched
# so its `signingUtils.sign` flushes the binary before reading it for signing.
# We override the concrete `buildPackages.darwin` derivations directly rather
# than via an overlay, because `darwin` is a spliced package set and overlay
# `//` additions to it do not survive splicing.  Empty list off aarch64-darwin.
{ lib, buildPackages, stdenv }:

lib.optional (stdenv.hostPlatform.isDarwin && stdenv.hostPlatform.isAarch64) (
  let
    # Tiny helper, built once in its own derivation (so no compile cost is paid
    # per sign, and elapsed build time cannot masquerade as the fix).
    fullfsync = buildPackages.runCommandCC "darwin-fullfsync" { } ''
      mkdir -p "$out/bin"
      cat > fullfsync.c <<'CEOF'
      #include <fcntl.h>
      #include <unistd.h>
      #include <stdio.h>
      int main(int argc, char **argv) {
        int rc = 0;
        for (int i = 1; i < argc; i++) {
          int fd = open(argv[i], O_RDONLY);
          if (fd < 0) { perror(argv[i]); rc = 1; continue; }
          if (fcntl(fd, F_FULLFSYNC) == -1) { perror("F_FULLFSYNC"); rc = 1; }
          fcntl(fd, F_NOCACHE, 1);
          static char buf[1 << 20];
          while (read(fd, buf, sizeof buf) > 0) { }
          close(fd);
        }
        return rc;
      }
      CEOF
      cc -O2 -Wall -o "$out/bin/fullfsync" fullfsync.c
    '';

    # Append a flush of the target to signingUtils.sign, immediately before it
    # reads the binary (overrideAttrs just adds a sed step to the concrete
    # derivation).  The flush failure aborts the build rather than being
    # swallowed (no `|| true`): signing an unflushed binary risks the very
    # invalid signature this works around.  The grep guard fails loudly if the
    # substitution did not take effect (e.g. upstream sign() changed) instead
    # of silently reintroducing the bug.
    signingUtils = buildPackages.darwin.signingUtils.overrideAttrs (old: {
      installPhase = old.installPhase + ''
        sed -i 's#cp "$1" "$tmpdir"#${fullfsync}/bin/fullfsync "$1"\n    cp "$1" "$tmpdir"#' $out
        grep -qF '${fullfsync}/bin/fullfsync' $out || {
          echo "darwin-codesign-flush: failed to patch signingUtils sign(); has upstream sign() changed?" >&2
          exit 1
        }
      '';
    });
  in
    buildPackages.darwin.autoSignDarwinBinariesHook.override { inherit signingUtils; }
)

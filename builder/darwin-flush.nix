# Fix for a darwin write->copy coherence problem (haskell.nix#2018).
#
# On a heavily-churned APFS build volume, reading a file shortly after it was
# written can return a stale recycled-block page instead of the bytes that were
# just written.  When a build step copies a freshly-produced file, the copy in
# $out can therefore differ from the bytes the producer actually wrote.
#
# We first hit this through code signing: on Apple Silicon ld64 builds the whole
# executable in an in-memory buffer, hashes that buffer for its ad-hoc signature,
# then write()s the same buffer to disk (it deliberately avoids mmap,
# rdar://66598213) — so the linked binary is valid.  But the later install copy
# read a stale page, so the binary in $out no longer matched ld64's embedded
# signature and macOS SIGKILLed it at startup ("Code Signature Invalid").
#
# The bad signature was only the *symptom* that made the corruption visible.  The
# underlying read returning stale bytes is not specific to executables or to
# aarch64: any copied file can be silently corrupted, and on x86_64-darwin
# (where these ad-hoc signatures aren't enforced) it would pass unnoticed.  So we
# flush *all* freshly-written files before they are copied, on both darwin arches.
#
# Cabal's own `copyFile` is no safer than `cp`: both copy via cached read()/write()
# (Cabal uses hGetBuf/hPutBuf, 4 KiB chunks, no mmap/fsync/clone), so both can
# read the stale page.  Rather than repair after the fact, we flush the
# freshly-written files to stable storage (F_FULLFSYNC) and drop their stale
# page-cache mapping (F_NOCACHE) *before* each copy, so the copy reads exactly
# the bytes that were written.
#
# Returns an attrset of shell-snippet helpers; all are empty off darwin:
#   flushDir  <dir>   flush every file under <dir>
#   flushFile <path>  flush a single file
# <dir> / <path> may contain shell variables (e.g. "$buildRoot/.../dist-newstyle").
{ lib, buildPackages, stdenv }:

let
  # Keyed off the *build* platform: the flush runs on the machine doing the
  # build — where the churned APFS volume and the F_FULLFSYNC/F_NOCACHE helper
  # live — not on the target.  Mirrors comp-builder.nix's `buildPlatform.isLinux`
  # `sync`.  Using buildPlatform also keeps the macOS-only C helper from being
  # compiled when cross-building *to* darwin from a non-darwin builder.
  enabled = stdenv.buildPlatform.isDarwin;

  # F_FULLFSYNC: macOS `sync`/`fsync` are async-weak on APFS; only F_FULLFSYNC
  # forces the bytes all the way to stable storage.  F_NOCACHE + a full read
  # then evicts the stale cached pages so the following copy misses the cache
  # and re-reads from disk.
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
        if (fcntl(fd, F_NOCACHE, 1) == -1) { perror("F_NOCACHE"); rc = 1; }
        static char buf[1 << 20];
        ssize_t n;
        while ((n = read(fd, buf, sizeof buf)) > 0) { }
        if (n < 0) { perror("read"); rc = 1; }
        close(fd);
      }
      return rc;
    }
    CEOF
    cc -O2 -Wall -o "$out/bin/fullfsync" fullfsync.c
  '';
in
{
  # Flush every freshly-written file under <dir> before it is copied.
  flushDir = dir: lib.optionalString enabled ''
    # haskell.nix#2018: flush freshly-written files before they are copied, so
    # the copy reads the producer's bytes and not a stale recycled-block page.
    if [ -d "${dir}" ]; then
      # Batch the paths through xargs rather than spawning one fullfsync per
      # file — dist-newstyle can hold thousands.
      find "${dir}" -type f -print0 | xargs -0 ${fullfsync}/bin/fullfsync
    fi
  '';

  # Flush a single freshly-written file before it is copied.
  flushFile = file: lib.optionalString enabled ''
    # haskell.nix#2018: see flushDir.
    if [ -e "${file}" ]; then ${fullfsync}/bin/fullfsync "${file}"; fi
  '';
}

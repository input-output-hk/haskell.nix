# Render the roots.ghc builds of a Hydra evaluation as a Markdown table.
#
# Input: the JSON array returned by Hydras /eval/<id>/builds endpoint.
# Output: raw Markdown (one table) on stdout.
#
# The table lists, per nixpkgs pin and per host-to-target platform, the build
# status of GHC itself (the roots.ghc job) for every compiler the CI currently
# evaluates.  Ordering is fully derived and deterministic so that an unchanged
# CI status produces byte-identical output (and hence no commit).
#
# NOTE: keep comments free of double quotes and backticks; jq 1.7+ parses a
# quote inside a comment as the start of a string literal.

# Known cross-target to human label.  Targets missing here still appear, using
# their raw Hydra name and sorting after all known targets, so a newly added
# target is never silently dropped from the table.
def target_label:
  { "native":                     "native"
  , "musl64":                     "musl64"
  , "musl32":                     "musl32"
  , "static":                     "static"
  , "aarch64-multiplatform":      "aarch64"
  , "aarch64-multiplatform-musl": "aarch64-musl"
  , "aarch64-android-prebuilt":   "android/aarch64"
  , "armv7a-android-prebuilt":    "android/armv7a"
  , "ghcjs":                      "javascript"
  , "wasi32":                     "wasm"
  , "mingwW64":                   "windows/mingwW64"
  , "ucrt64":                     "windows/ucrt64"
  } as $m | $m[.] // .;

def target_rank:
  { "native": 0, "static": 1, "musl64": 2, "musl32": 3
  , "aarch64-multiplatform": 4, "aarch64-multiplatform-musl": 5
  , "aarch64-android-prebuilt": 6, "armv7a-android-prebuilt": 7
  , "ghcjs": 8, "wasi32": 9, "mingwW64": 10, "ucrt64": 11
  } as $m | $m[.] // 999;

def system_rank:
  { "x86_64-linux": 0, "aarch64-linux": 1, "aarch64-darwin": 2, "x86_64-darwin": 3 }
  as $m | $m[.] // 999;

# unstable first, then any other pins alphabetically
def pin_rank:
  { "unstable": 0 } as $m | $m[.] // 500;

# Status cell for one build.  $latest is the id of the most recent evaluation
# (or null if it could not be determined).
#
# Freshness: a cell only reflects a build that belongs to the current
# evaluation.  Because Nix reuses an unchanged derivation across evaluations,
# an unchanged compiler's current build carries the latest evaluation id in
# .jobsetevals; a changed compiler gets a new build in that evaluation (still
# building, or already failed).  So if the job's latest build does NOT carry
# the latest evaluation id, the current evaluation simply does not build this
# job (it was dropped from the matrix), and we render it the same as a job that
# never existed (a dot) rather than advertising a stale green.  When $latest is
# null the check is skipped and the latest build is reported as-is.
#
# Hydra buildstatus (only meaningful when finished == 1):
#   0 success, 1 failed, 2 dependency failed, 3 aborted, 4 cancelled,
#   6 failed with output, anything else treated as failed.
def status_cell($latest):
  ( $latest == null or ([ .jobsetevals[]? ] | any(. == $latest)) ) as $fresh
  | if ($fresh | not) then "·"
    elif .finished == 0 or .finished == null then "🕓"
    elif .buildstatus == 0 then "✅"
    elif .buildstatus == 2 then "⚠️"
    elif (.buildstatus == 3 or .buildstatus == 4) then "⚪"
    else "❌"
    end;

# Sort key for a compiler name such as ghc9141 or ghc9141llvm: primary is the
# numeric part as an integer, secondary puts llvm variants last.
def compiler_key:
  ([ match("[0-9]+").string | tonumber ]) + [ (if test("llvm") then 1 else 0 end) ];

[ .[]
  | select(.job | test("\\.roots\\.ghc$"))
  | (.job | split(".")) as $p
  | { pin: $p[1], system: $p[0], compiler: $p[2], target: $p[3]
    , cell: status_cell($latest_eval) }
] as $jobs

| ($jobs | map(.compiler) | unique | sort_by(compiler_key)) as $compilers

| ( $jobs
    | map({ pin, system, target })
    | unique
    | sort_by([ (.pin | pin_rank), .pin, (.system | system_rank), .system
              , (.target | target_rank), .target ])
  ) as $rows

# cell lookup keyed by pin, system, target, compiler joined with spaces
| ( $jobs
    | map({ key: ([.pin, .system, .target, .compiler] | join(" ")), value: .cell })
    | from_entries
  ) as $lookup

| ( [ "Nixpkgs", "Host → Target" ] + $compilers ) as $header
| ( [ "| " + ($header | join(" | ")) + " |"
    , "| " + ($header | map("---") | join(" | ")) + " |"
    ]
    + ( $rows | map(
          . as $r
          | ( if $r.target == "native"
              then "`" + $r.system + "`"
              else "`" + $r.system + "` → " + ($r.target | target_label)
              end ) as $plat
          | "| " + $r.pin + " | " + $plat + " | "
            + ( $compilers
                | map( $lookup[ ([$r.pin, $r.system, $r.target, .] | join(" ")) ] // "·" )
                | join(" | ") )
            + " |"
      ))
  )
| join("\n")

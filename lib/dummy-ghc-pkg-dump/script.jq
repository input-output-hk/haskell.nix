def pkg_id:
  "\(.name)-\(.version)"
  ;

def build_depends($inputs):
  ."build-depends" | map(.package | strings | $inputs[.] | pkg_id) | join(" ")
  ;

def exposed_modules:
    ."exposed-modules"
  | map(strings)
  | join(" ")
  ;

[ inputs | { key: "\(.name)", value: . } ]
| from_entries as $inputs

| $inputs
| map(
    .name as $name
  | .version as $version
  | .components
  | to_entries
  | map(
    # only libraries
      select(.key | startswith("lib"))
    | .value
    | "name: \($name)"
    , "version: \($version)"
    , "exposed-modules: \(exposed_modules)"
    , "build-depends: \(build_depends($inputs))"
  ) | join("\n")
) | join("\n---\n")

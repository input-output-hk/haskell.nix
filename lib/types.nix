{ lib }:

with lib;

rec {
  # This is just like listOf, except that it filters out all null elements.
  listOfFilteringNulls = elemType: types.listOf elemType // {
    # Mostly copied from nixpkgs/lib/types.nix
    merge = loc: defs:
      map (x: x.value) (filter (x: x ? value && x.value != null) (concatLists (imap1
        (n: def:
          if isList def.value then
            imap1
              (m: def':
                (mergeDefinitions
                  (loc ++ [ "[definition ${toString n}-entry ${toString m}]" ])
                  elemType
                  [{ inherit (def) file; value = def'; }]
                ).optionalValue
              )
              def.value
          else
            throw "The option value `${showOption loc}` in `${def.file}` is not a list.")
        defs)));
  };

  # dealing with str is a bit annoying especially with `nullOr str` as that apparently defaults to ""
  # instead of null :shrug:.  This then messes with our option inheritance logic.
  # Hence we have a uniqueStr type that ensures multiple identically defined options are collapsed
  # without raising an error. And a way to fetch default options that will retain `null` if the
  # option is not defined or "".
  getDefaultOrNull = def: key: if def ? ${key} && def.${key} != "" then def.${key} else null;

  mergeUniqueOption = locs: defs:
    let
      mergeOneOption = loc: defs':
        # we ignore "" as optionalString, will default to "".
        let defs = filter (x: x.value != "") defs'; in
        if defs == [ ] then null
        else if length defs != 1 then
          throw "The unique option `${showOption loc}' is defined multiple times, in ${showFiles (getFiles defs)}; with values `${concatStringsSep "', `" (map (x: x.value) defs)}'."
        else (head defs).value;
    in
    mergeOneOption locs (lists.unique defs);

  uniqueStr = types.str // { merge = mergeUniqueOption; };
}

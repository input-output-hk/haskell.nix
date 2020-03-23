# This is a replacement for the cleanSourceWith function in nixpkgs
# https://github.com/NixOS/nixpkgs/blob/1d9d31a0eb8e8358830528538a391df52f6a075a/lib/sources.nix#L41
# It adds a subDir argument in a way that allows descending into a subdirectory
# to compose with cleaning the source with a filter.
{ lib }: rec {

  # Like `builtins.filterSource`, except it will compose with itself,
  # allowing you to chain multiple calls together without any
  # intermediate copies being put in the nix store.
  #
  #     lib.cleanSourceWith {
  #       filter = f;
  #       src = lib.cleanSourceWith {
  #         filter = g;
  #         src = ./.;
  #       };
  #     }
  #     # Succeeds!
  #
  #     builtins.filterSource f (builtins.filterSource g ./.)
  #     # Fails!
  #
  # Parameters:
  #
  #   src:      A path or cleanSourceWith result to filter and/or rename.
  #
  #   filter:   A function (path -> type -> bool)
  #             Optional with default value: constant true (include everything)
  #             The function will be combined with the && operator such
  #             that src.filter is called lazily.
  #             For implementing a filter, see
  #             https://nixos.org/nix/manual/#builtin-filterSource
  #
  #   subDir:   Descend into a subdirectory in a way that will compose.
  #             It will be as if `src = src + "/${subDir}` and filters
  #             already applied to `src` will be respected.
  #
  #   name:     Optional name to use as part of the store path.
  #             If you do not provide a `name` it wil be derived
  #             from the `subDir`. You should provide `name` or
  #             `subDir`.  If you do not a warning will be displayed
  #             and the name used will be `source-${caller}`.
  #
  #   caller:   Name of the function used in warning message and
  #             in the default `source-${caller}` name.  Functions
  #             that are implemented using `cleanSourceWith` (and
  #             forward a `name` argument) can use this to make
  #             the message to the use more meaningful.
  #
  cleanSourceWith = { filter ? _path: _type: true, src, subDir ? "", name ? null
      , caller ? "cleanSourceWith" }:
    let
      subDir' = if subDir == "" then "" else "/" + subDir;
      subDirName = __replaceStrings ["/"] ["-"] subDir;
      # In case this is mixed with older versions of cleanSourceWith
      isFiltered = src ? _isLibCleanSourceWith;
      isFilteredEx = src ? _isLibCleanSourceWithEx;
      origSrc = if isFiltered || isFilteredEx then src.origSrc else src;
      origSubDir = if isFilteredEx then src.origSubDir + subDir' else subDir';
      origSrcSubDir = toString origSrc + origSubDir;
      parentFilter = if isFiltered || isFilteredEx
        then path: type: src.filter path type
        else path: type: true;
      filter' = path: type:
        # Include parent paths based on the parent filter
           (lib.strings.hasPrefix (path + "/") (origSrcSubDir + "/")
            && parentFilter path type)
        # Children only if both filters return true
        || (lib.strings.hasPrefix (origSrcSubDir + "/") path
            && (filter path type && parentFilter path type));
      name' = if name != null
        then name
        else
          if subDirName != ""
            then if src ? name
              then src.name + "-" + subDirName 
              else "source-${caller}-" + subDirName
            else if src ? name
              then src.name
              else
                # No name was provided and one could not be constructed from
                # the `subDirName`.

                # We used to use `baseNameOf src` as a default here.
                # This was cute, but it lead to cache misses.  For instance if
                # `x = cleanSourceWith { src = ./.; }` then `baseName src`
                # will be different when `src` resolves to "/nix/store/X"
                # than when it is in a local directory.  If people use
                # git worktrees they also may wind up with different
                # values for `name`. Anything that depends on `x.name` will
                # propagate the issue.

                # Encourage adding a suitable `name` with:
                #   * A warning message.
                #   * A default name that gives a hint as to why there is no name.
                __trace (
                    "WARNING: `${caller}` called on ${toString src} without a `name`. "
                    + "Consider adding `name = \"${baseNameOf src};\"`") "source-${caller}";
    in {
      inherit origSrc origSubDir origSrcSubDir;
      filter = filter';
      outPath = (builtins.path { filter = filter'; path = origSrc; name = name'; }) + origSubDir;
      _isLibCleanSourceWithEx = true;
      # It is only safe for older cleanSourceWith to filter this one
      # if the we are still looking at the root of origSrc
      _isLibCleanSourceWith = origSubDir == "";
      name = name';
    };

  pathHasContext = builtins.hasContext or (lib.hasPrefix builtins.storeDir);

  canCleanSource = src:
       src ? _isLibCleanSourceWithEx
    || src ? _isLibCleanSourceWith
    || !(pathHasContext (toString src));
}
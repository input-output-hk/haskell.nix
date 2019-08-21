# This is a replacement for the cleanSourceWith function in nixpkgs
# https://github.com/NixOS/nixpkgs/blob/1d9d31a0eb8e8358830528538a391df52f6a075a/lib/sources.nix#L41
# It adds a subDir argument in a way that allows descending into a subdirectory
# to compose with cleaning the source with a filter.
{ lib }: rec {
  cleanSourceWith = { filter ? path: type: true, src, subDir ? "" }:
    let
      subDir' = if subDir == "" then "" else "/" + subDir;
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
    in {
      inherit origSrc origSubDir origSrcSubDir;
      filter = filter';
      outPath = (builtins.filterSource filter' origSrc) + origSubDir;
      _isLibCleanSourceWithEx = true;
      # It is only safe for older cleanSourceWith to filter this one
      # if the we are still looking at the root of origSrc
      _isLibCleanSourceWith = origSubDir == "";
    };

  pathHasContext = builtins.hasContext or (lib.hasPrefix builtins.storeDir);

  canCleanSource = src:
       src ? _isLibCleanSourceWithEx
    || src ? _isLibCleanSourceWith
    || !(pathHasContext (toString src));
}
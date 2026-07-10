# Find the resolver in the stack.yaml file and fetch it if a sha256 value is provided
{ pkgs }:
{ src
, stackYaml ? "stack.yaml"
, resolverSha256 ? null
}:
let
    srcDir = src.origSrcSubDir or src;

    # Using origSrcSubDir bypasses any cleanSourceWith so that it will work when
    # access to the store is restricted.  If origSrc was already in the store
    # you can pass the project in as a string.
    rawStackYaml = builtins.readFile (srcDir + "/${stackYaml}");

    # Determine the resolver as it may point to another file we need
    # to look at.  `snapshot` is the modern synonym for `resolver`
    # (stack >= 2.15.1), so accept either key.
    resolver =
      let
        # Each returns a single-element capture list (the value) or null.
        matchLine = l:
          let r = builtins.match "^resolver: *(.*)" l;
          in if r != null then r else builtins.match "^snapshot: *(.*)" l;
        rs = pkgs.lib.lists.concatLists (
          pkgs.lib.lists.filter (l: l != null)
            (builtins.map matchLine
              (pkgs.lib.splitString "\n" rawStackYaml)));
      in
        pkgs.lib.lists.head (rs ++ [ null ]);

    # If we found a resolver and we have a resolverSha256 then we should download it.
    fetchedResolver =
      if resolver != null && resolverSha256 != null
        then pkgs.fetchurl {
          url = resolver;
          sha256 = resolverSha256;
        }
      else if resolver != null && __pathExists (srcDir + "/${resolver}")
        then pkgs.copyPathToStore (srcDir + "/${resolver}")
      else null;

in { inherit resolver fetchedResolver; }

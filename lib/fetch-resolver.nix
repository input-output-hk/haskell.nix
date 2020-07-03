# Find the resolver in the stack.yaml file and fetch it if a sha256 value is provided
{ pkgs }:
{ src
, stackYaml ? "stack.yaml"
, resolverSha256 ? null
}:
let
    # Using origSrcSubDir bypasses any cleanSourceWith so that it will work when
    # access to the store is restricted.  If origSrc was already in the store
    # you can pass the project in as a string.
    rawStackYaml = builtins.readFile ((src.origSrcSubDir or src) + ("/" + stackYaml));

    # Determine the resolver as it may point to another file we need
    # to look at.
    resolver =
      let
        rs = pkgs.lib.lists.concatLists (
          pkgs.lib.lists.filter (l: l != null)
            (builtins.map (l: builtins.match "^resolver: *(.*)" l)
              (pkgs.lib.splitString "\n" rawStackYaml)));
      in
        pkgs.lib.lists.head (rs ++ [ null ]);

    # If we found a resolver andwe have a resolverSha256 then we should download it.
    fetchedResolver =
      if resolver != null && resolverSha256 != null
        then pkgs.fetchurl {
          url = resolver;
          sha256 = resolverSha256;
        }
        else null;

in { inherit resolver fetchedResolver; }

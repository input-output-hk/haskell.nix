# Find the resolver in the stack.yaml file and fetch it if a sha256 value is provided
{ pkgs }:
{ src
, stackYaml ? "stack.yaml"
, defaultResolver ? "resolver: lts-16.31" # default resolver is given to fix null case
, resolverSha256 ? null
}:
let
    # Using origSrcSubDir bypasses any cleanSourceWith so that it will work when
    # access to the store is restricted.  If origSrc was already in the store
    # you can pass the project in as a string.
  rawStackYaml = builtins.readFile ((src.origSrcSubDir or src) + ("/" + stackYaml));
   
    defaultResolver = "resolver: lts-16.31";
    # Determine the resolver as it may point to another file we need
    # to look at.
    resolver =
      let 
        yamlList = pkgs.lib.splitString "\n" rawStackYaml;
        resolverFind = pkgs.lib.foldl' (l: a: if builtins.isNull (builtins.match "^resolver: *(.*)" a)
                                              then defaultResolver
                                              else a
        ) [] yamlList;
        concatLists = builtins.concatLists resolverFind ;
      in "resolver: lts-16.31"; # builtins.trace resolverFind resolverFind;

    # resolver =
    #   let
    #     rs = pkgs.lib.lists.concatLists (
    #       pkgs.lib.lists.filter (l: l != null)
    #         (builtins.trace "*(.*)" (builtins.map (l: builtins.match "^resolver: *(.*)" l))
    #           (builtins.trace ( rawStackYaml)  (pkgs.lib.splitString "\n" rawStackYaml))));
    #   in
    #     builtins.trace ("get resolver") (pkgs.lib.lists.head ( (builtins.trace "rs" rs) ++ [ null ]));

    # If we found a resolver and we have a resolverSha256 then we should download it.
    fetchedResolver =
      if resolver != null && resolverSha256 != null
        then pkgs.fetchurl {
          url = resolver;
          sha256 = resolverSha256;
        }
        else null;





  
    



in { inherit resolver fetchedResolver; }

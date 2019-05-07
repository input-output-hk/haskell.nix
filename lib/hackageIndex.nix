{ runCommand, cabal-install
, indexState ? "2019-04-24T21:34:04Z"
} :
let
  # To avoid downloading more data than necessary this will provide a base.
  cachedState = runCommand "hackage-${builtins.substring 0 4 indexState}" {} ''
    mkdir -p $out
    HOME=$out ${cabal-install}/bin/cabal new-update 'hackage.haskell.org,${builtins.substring 0 4 indexState}-01-01T00:00:00Z'
  '';
in runCommand "hackage-${builtins.replaceStrings [":"] [""] indexState}" {} ''
    mkdir -p $out
    cp -r ${cachedState}/.cabal $out
    chmod +w -R $out/.cabal
    sed -i.back -e "s|${cachedState}|$out|g" $out/.cabal/config
    HOME=$out ${cabal-install}/bin/cabal new-update 'hackage.haskell.org,${indexState}'
  ''

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cabal2Nix.Plan
where

import           Cabal2Nix.Util                           ( quoted
                                                          , bindPath
                                                          )
import           Data.HashMap.Strict                      ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.List.NonEmpty                       ( NonEmpty (..) )
import           Data.Text                                ( Text )
import qualified Data.Text                     as Text
import           Nix.Expr

type Version = Text
type Revision = Text -- Can be: rNUM, cabal file sha256, or "default"

data Plan = Plan
  { packages :: HashMap Text Package
  , compilerVersion :: Text
  , compilerPackages :: HashMap Text (Maybe Version)
  }

data Package = Package
  { packageVersion :: Version
  , packageRevision :: Maybe Revision
  , packageFlags :: HashMap Text Bool
  }

plan2nix :: Plan -> NExpr
plan2nix (Plan { packages, compilerVersion, compilerPackages }) =
  mkFunction "hackage"
    . mkNonRecSet
    $ [ "packages" $= (mkNonRecSet $ uncurry bind =<< Map.toList packages)
      , "compiler" $= mkNonRecSet
        [ "version" $= mkStr compilerVersion
        , "nix-name" $= mkStr ("ghc" <> Text.filter (/= '.') compilerVersion)
        , "packages" $= mkNonRecSet (uncurry bind' <$> Map.toList compilerPackages)
        ]
        -- overlay = pkgs: self: super: with pkgs.haskell.lib; { ... = dontCheck ... };
      , "overlay" $= ( mkFunction "pkgs"
                     . mkFunction "self"
                     . mkFunction "super"
                     . mkWith (mkSym "pkgs" @. "haskell" @. "lib")
                     -- a null-ignoring dontCheck:
                     -- let dontCheck' = x:
                     --   if builtins.hasAttr "override" x
                     --     then dontCheck x
                     --     else x
                     . mkLets [ "dontCheck'" $= mkFunction "x"
                                (mkIf ( (mkSym "builtins" @. "hasAttr") @@ mkStr "override" @@ mkSym "x" )
                                      ( mkSym "dontCheck" @@ mkSym "x" )
                                      ( mkSym "x" )
                                      ) ]
                     . mkNonRecSet
                     $ fmap (uncurry bindTo) . Map.toList $ (\k _v -> mkSym "dontCheck'" @@ (mkSym "super" @. k)) `Map.mapWithKey` packages)
      ]
 where
  bind pkg (Package { packageVersion, packageRevision, packageFlags }) =
    let verExpr      = mkSym "hackage" @. pkg @. quoted packageVersion
        revExpr      = verExpr @. "revisions" @. maybe "default" quoted packageRevision
        revBinding   = bindPath (pkg :| ["revision"]) revExpr
        flagBindings = Map.foldrWithKey
          (\fname val acc -> bindPath (pkg :| ["flags", fname]) (mkBool val) : acc)
          []
          packageFlags
    in  revBinding : flagBindings
  bind' pkg ver = quoted pkg $= maybe mkNull mkStr ver

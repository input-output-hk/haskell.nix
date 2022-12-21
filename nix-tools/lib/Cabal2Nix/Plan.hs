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

type PackageId = Text
type InstantiatedWith = Text

-- | Extracted info about instantiated signatures in the libraries.
--
-- The keys are package ids and the values are instantiated-with lines.
-- For example, '<signature-name>=<pkg-id>:<module-name>',
-- where <pkg-id> is '<pkg-name>-<pkg-version>-inplace-<library-name>'.
--
-- Package ids as keys have abis instead of 'inplace'.
newtype InstantiatedWithMap = InstantiatedWithMap
  { unInstantiatedMapWith :: HashMap PackageId [InstantiatedWith]
  } deriving (Eq, Show)

emptyInstantiatedWithMap :: InstantiatedWithMap
emptyInstantiatedWithMap = InstantiatedWithMap Map.empty

data Plan = Plan
  { packages :: HashMap Text (Maybe Package)
  , compilerVersion :: Text
  , compilerPackages :: HashMap Text (Maybe Version)
  }

data Package = Package
  { packageVersion :: Version
  , packageRevision :: Maybe Revision
  , packageFlags :: HashMap VarName Bool
  }

plan2nix :: Plan -> NExpr
plan2nix (Plan { packages, compilerVersion, compilerPackages }) =
  mkFunction "hackage"
    . mkNonRecSet
    $ [ "packages" $= (mkNonRecSet $ uncurry bind =<< Map.toList quotedPackages)
      , "compiler" $= mkNonRecSet
        [ "version" $= mkStr compilerVersion
        , "nix-name" $= mkStr ("ghc" <> Text.filter (/= '.') compilerVersion)
        , "packages" $= mkNonRecSet (fmap (uncurry bind') $ Map.toList $ mapKeys quoted compilerPackages)
        ]
      ]
 where
  quotedPackages = mapKeys quoted packages
  bind :: Text -> Maybe Package -> [Binding NExpr]
  bind pkg (Just (Package { packageVersion, packageRevision, packageFlags })) =
    let verExpr      = (mkSym "hackage" @. pkg) @. quoted packageVersion
        revExpr      = (verExpr @. "revisions") @. maybe "default" quoted packageRevision
        flagBindings = Map.foldrWithKey
          (\fname val acc -> bindPath (VarName pkg :| ["flags", fname]) (mkBool val) : acc)
          []
          packageFlags
    in  revBinding pkg revExpr : flagBindings
  bind pkg Nothing = [revBinding pkg mkNull]
  revBinding :: Text -> NExpr -> Binding NExpr
  revBinding pkg revExpr = bindPath (VarName pkg :| ["revision"]) revExpr
  bind' pkg ver = pkg $= maybe mkNull mkStr ver
  mapKeys f = Map.fromList . fmap (\(k, v) -> (f k, v)) . Map.toList

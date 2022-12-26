module ImplA where

data SomeData = SomeData String

instance Show SomeData where
  show (SomeData s) = s

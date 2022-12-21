module ImplC where

data SomeData = SomeData Int

instance Show SomeData where
  show (SomeData s) = show s

module LibSpec
    ( spec
    ) where

import Test.Hspec
import Lib

spec :: Spec
spec = describe "Lib message" $
  it "should greet appropriately" $
    message `shouldContain` "Hello"

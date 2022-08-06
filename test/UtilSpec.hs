{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UtilSpec (spec) where

import Import
import Test.Hspec
import Util

spec :: Spec
spec = do
  describe "replaceFirst" $ do
    it "basic check" $ replaceFirst "x" "y" "xx" `shouldBe` "yx"

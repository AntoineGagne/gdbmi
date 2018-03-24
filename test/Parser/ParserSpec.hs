{-# LANGUAGE OverloadedStrings #-}

module Parser.ParserSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Parsec
    ( parse )

import Parser.Types
import Parser.Printer

import qualified Parser.Parser as Parser

spec :: Spec
spec = do
    tokenSpec
    resultClassSpec

tokenSpec :: Spec
tokenSpec =
    describe "token" $
        context "given valid token input" $
            prop "returns the same token as the input" prop_validToken

prop_validToken :: Positive Integer -> Bool
prop_validToken (Positive n) 
    = case parse Parser.token "" (showToken (Just n)) of
          Left _ -> False
          Right token' -> token' == n

resultClassSpec :: Spec
resultClassSpec =
    describe "resultClass" $
        context "given valid result class input" $
            prop "returns the same result class as the input" prop_validResultClass

prop_validResultClass :: ResultClass -> Bool
prop_validResultClass resultClass'
    = case parse Parser.resultClass "" (showResultClass resultClass') of
          Left _ -> False
          Right resultClass'' -> resultClass'' == resultClass'

instance Arbitrary ResultClass where
    arbitrary = do
        n <- choose (0, 4) :: Gen Integer
        pure $ case n of
            0 -> Done
            1 -> Running
            2 -> Connected
            3 -> Error
            4 -> Exit

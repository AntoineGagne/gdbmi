{-# LANGUAGE OverloadedStrings #-}

module Parser.ParserSpec where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec
    ( parse )

import Parser.Types
import Parser.Printer

import qualified Parser.Parser as Parser

spec :: Spec
spec = tokenSpec

tokenSpec :: Spec
tokenSpec =
    describe "token" $
        context "when parsing token" $
            it "returns a parsable token" $ property prop_valid_token

prop_valid_token :: Positive Integer -> Bool
prop_valid_token (Positive n) 
    = case parse Parser.token "" (showToken (Just n)) of
          Left _ -> False
          Right token' -> token' == n

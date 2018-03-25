{-# LANGUAGE OverloadedStrings #-}

module Parser.ParserSpec where

import Data.Text

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Text
import Text.Megaparsec
    ( parse )

import Parser.Types
import Parser.Printer

import qualified Parser.Parser as Parser

spec :: Spec
spec = do
    tokenSpec
    resultClassSpec
    streamRecordSpec

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

streamRecordSpec :: Spec
streamRecordSpec =
    describe "streamRecord" $
        context "given valid stream record input" $
            prop "returns the same stream record as the input" prop_validStreamRecord

prop_validStreamRecord :: StreamRecord -> Bool
prop_validStreamRecord streamRecord'
    = case parse Parser.streamRecord "" (showStreamRecord streamRecord') of
          Left _ -> False
          Right streamRecord'' -> streamRecord'' == streamRecord'

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

instance Arbitrary StreamRecord where
    arbitrary = do
        n <- choose (0, 2) :: Gen Integer
        text <- getPossibleText <$> (arbitrary :: Gen PossibleText)
        pure $ case n of
            0 -> ConsoleStreamOutput text
            1 -> TargetStreamOutput text
            2 -> LogStreamOutput text

newtype PossibleText = PossibleText
    { getPossibleText :: Text }

instance Arbitrary PossibleText where
    arbitrary = PossibleText 
            <$> oneof [ pure Data.Text.empty
                      , pure "Test"
                      , pure "\\\"Test\\\""
                      , pure "\\nTest\\\""
                      , pure "\\rTest\\v"
                      , pure "\\v\\tTest\\b\\f"
                      , pure "\\\\test"
                      , pure "~&@Test"
                      , pure "Un test avec des caractères unicodes. \
                             \Peut-être que ça ne marchera pas."
                      ]



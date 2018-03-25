{-# LANGUAGE OverloadedStrings #-}

module Parser.ParserSpec where

import Data.Text

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding
    ( Result )
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
    listSpec
    resultSpec
    valueSpec

tokenSpec :: Spec
tokenSpec =
    describe "token" $
        context "given valid token input" $
            prop "returns the same token as the input" prop_idempotenceOfTokenParsing

prop_idempotenceOfTokenParsing :: Positive Integer -> Bool
prop_idempotenceOfTokenParsing (Positive n) 
    = case parse Parser.token "" (showToken (Just n)) of
          Left _ -> False
          Right token' -> token' == n

streamRecordSpec :: Spec
streamRecordSpec =
    describe "streamRecord" $
        context "given valid stream record input" $
            prop "returns the same stream record as the input" prop_idempotenceOfStreamRecordParsing

prop_idempotenceOfStreamRecordParsing :: StreamRecord -> Bool
prop_idempotenceOfStreamRecordParsing streamRecord'
    = case parse Parser.streamRecord "" (showStreamRecord streamRecord') of
          Left _ -> False
          Right streamRecord'' -> streamRecord'' == streamRecord'

resultClassSpec :: Spec
resultClassSpec =
    describe "resultClass" $
        context "given valid result class input" $
            prop "returns the same result class as the input" prop_idempotenceOfResultClassParsing

prop_idempotenceOfResultClassParsing :: ResultClass -> Bool
prop_idempotenceOfResultClassParsing resultClass'
    = case parse Parser.resultClass "" (showResultClass resultClass') of
          Left _ -> False
          Right resultClass'' -> resultClass'' == resultClass'

listSpec :: Spec
listSpec =
    describe "list" $
        context "given valid list input" $
            prop "returns the same list as the input" prop_idempotenceOfListParsing

prop_idempotenceOfListParsing :: List -> Bool
prop_idempotenceOfListParsing list'
    = case parse Parser.list "" (showList' list') of
          Left _ -> False
          Right list'' -> list'' == list'

resultSpec :: Spec
resultSpec =
    describe "result" $
        context "given valid result input" $
            prop "returns the same result as the input" prop_idempotenceOfResultParsing

prop_idempotenceOfResultParsing :: Result -> Bool
prop_idempotenceOfResultParsing result'
    = case parse Parser.result "" (showResult result') of
          Left _ -> False
          Right result'' -> result'' == result'

valueSpec :: Spec
valueSpec =
    describe "value" $
        context "given valid value input" $
            prop "returns the same value as the input" prop_idempotenceOfValueParsing

prop_idempotenceOfValueParsing :: Value -> Bool
prop_idempotenceOfValueParsing value'
    = case parse Parser.value "" (showValue value') of
          Left _ -> False
          Right value'' -> value'' == value'

instance Arbitrary List where
    arbitrary = sized $ \n -> frequency
        [ (n, pure EmptyList)
        , (1, ValueList <$> vectorOf (n + 1) (arbitrary :: Gen Value))
        , (1, ResultList <$> vectorOf (n + 1) (arbitrary :: Gen Result))
        ]

instance Arbitrary Result where
    arbitrary = do
        variable <- getPossibleVariableName <$> (arbitrary :: Gen PossibleVariableName)
        value <- arbitrary :: Gen Value
        pure $ Result variable value

instance Arbitrary Value where
    arbitrary = sized $ \n -> frequency
        [ (n, Const <$> (getPossibleText <$> (arbitrary :: Gen PossibleText)))
        , (1, Tuple <$> vectorOf n (arbitrary :: Gen Result))
        , (1, VList <$> (arbitrary :: Gen List))
        ]

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

newtype PossibleVariableName = PossibleVariableName
    { getPossibleVariableName :: Text }

instance Arbitrary PossibleVariableName where
    arbitrary = PossibleVariableName
        <$> oneof [ pure "id"
                  , pure "test-id"
                  , pure "test_id"
                  , pure "test-id_1"
                  , pure "1test_id_2"
                  ]

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

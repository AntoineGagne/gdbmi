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
    asyncClassSpec
    asyncOutputSpec
    asyncRecordSpec
    resultRecordSpec
    outputSpec

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

asyncClassSpec :: Spec
asyncClassSpec =
    describe "asyncClass" $
        context "given valid async class input" $
            prop "returns the same async class as the input" prop_idempotenceOfAsyncClassParsing

prop_idempotenceOfAsyncClassParsing :: AsyncClass -> Bool
prop_idempotenceOfAsyncClassParsing asyncClass'
    = case parse Parser.asyncClass "" (showAsyncClass asyncClass') of
          Left _ -> False
          Right asyncClass'' -> asyncClass'' == asyncClass'

asyncOutputSpec :: Spec
asyncOutputSpec =
    describe "asyncOutput" $
        context "given valid async output input" $
            prop "returns the same async output as the input" prop_idempotenceOfAsyncOutputParsing

prop_idempotenceOfAsyncOutputParsing :: AsyncOutput -> Bool
prop_idempotenceOfAsyncOutputParsing asyncOutput'
    = case parse Parser.asyncOutput "" (showAsyncOutput asyncOutput') of
          Left _ -> False
          Right asyncOutput'' -> asyncOutput'' == asyncOutput'

asyncRecordSpec :: Spec
asyncRecordSpec =
    describe "asyncRecord" $
        context "given valid async record input" $
            prop "returns the same async record as the input" prop_idempotenceOfAsyncRecordParsing

prop_idempotenceOfAsyncRecordParsing :: AsyncRecord -> Bool
prop_idempotenceOfAsyncRecordParsing asyncRecord'
    = case parse Parser.asyncRecord "" (showAsyncRecord asyncRecord') of
          Left _ -> False
          Right asyncRecord'' -> asyncRecord'' == asyncRecord'

outOfBandRecordSpec :: Spec
outOfBandRecordSpec =
    describe "outOfBandRecord" $
        context "given valid out of band record input" $
            prop "returns the same out of band record as the input" prop_idempotenceOfOutOfBandRecordParsing

prop_idempotenceOfOutOfBandRecordParsing :: OutOfBandRecord -> Bool
prop_idempotenceOfOutOfBandRecordParsing outOfBandRecord'
    = case parse Parser.outOfBandRecord "" (showOutOfBandRecord outOfBandRecord') of
          Left _ -> False
          Right outOfBandRecord'' -> outOfBandRecord'' == outOfBandRecord'

resultRecordSpec :: Spec
resultRecordSpec =
    describe "resultRecord" $
        context "given valid result record input" $
            prop "returns the same result record as the input" prop_idempotenceOfResultRecordParsing

prop_idempotenceOfResultRecordParsing :: ResultRecord -> Bool
prop_idempotenceOfResultRecordParsing resultRecord'
    = case parse Parser.resultRecord "" (showResultRecord resultRecord') of
          Left _ -> False
          Right resultRecord'' -> resultRecord'' == resultRecord'

outputSpec :: Spec
outputSpec =
    describe "output" $
        context "given valid output input" $
            prop "returns the same output as the input" prop_idempotenceOfOutputParsing

prop_idempotenceOfOutputParsing :: Output -> Bool
prop_idempotenceOfOutputParsing output'
    = case parse Parser.output "" (showOutput output') of
          Left _ -> False
          Right output'' -> output'' == output'

instance Arbitrary Output where
    arbitrary = do
        outOfBandRecords <- arbitrary :: Gen [OutOfBandRecord]
        resultRecord <- arbitrary :: Gen (Maybe ResultRecord)
        pure $ Output outOfBandRecords resultRecord

instance Arbitrary ResultRecord where
    arbitrary = do
        token <- arbitrary :: Gen (Maybe (Positive Integer))
        resultClass <- arbitrary :: Gen ResultClass
        results <- arbitrary :: Gen [Result]
        pure $ ResultRecord (getPositive <$> token) resultClass results


instance Arbitrary OutOfBandRecord where
    arbitrary = do
        n <- choose (0, 1) :: Gen Integer
        case n of
            0 -> OutOfBandAsyncRecord <$> (arbitrary :: Gen AsyncRecord)
            1 -> OutOfBandStreamRecord <$> (arbitrary :: Gen StreamRecord)

instance Arbitrary AsyncRecord where
    arbitrary = do
        n <- choose (0, 2) :: Gen Integer
        token <- arbitrary :: Gen (Maybe (Positive Integer))
        asyncOutput <- arbitrary :: Gen AsyncOutput
        pure $ case n of
            0 -> ExecAsyncOutput (getPositive <$> token) asyncOutput
            1 -> StatusAsyncOutput (getPositive <$> token) asyncOutput
            2 -> NotifyAsyncOutput (getPositive <$> token) asyncOutput

instance Arbitrary AsyncOutput where
    arbitrary = do
        asyncClass <- arbitrary :: Gen AsyncClass
        asyncResults <- arbitrary :: Gen [Result]
        pure $ AsyncOutput asyncClass asyncResults

instance Arbitrary AsyncClass where
    arbitrary = do
        n <- choose (0, 8) :: Gen Integer
        pure $ case n of
            0 -> Stopped
            1 -> ThreadGroupAdded
            2 -> ThreadGroupStarted
            3 -> ThreadCreated
            4 -> AsyncClassRunning
            5 -> ThreadExited
            6 -> ThreadGroupExited
            7 -> BreakpointModified
            8 -> LibraryLoaded

instance Arbitrary List where
    arbitrary = sized $ \n -> frequency
        [ (n, pure EmptyList)
        , (1, ValueList <$> vectorOf (n + 1) (arbitrary :: Gen Value))
        , (1, ResultList <$> generateResults (n + 1))
        ]

generateResults :: Int -> Gen [Result]
generateResults 0 = pure []
generateResults n = 
  (:) 
    <$> ( Result 
        <$> (getPossibleVariableName
            <$> (arbitrary :: Gen PossibleVariableName)
            )
        <*> (Const . getPossibleText <$> (arbitrary :: Gen PossibleText))
        ) 
    <*> generateResults (n - 1)

instance Arbitrary Result where
    arbitrary = do
        variable <- getPossibleVariableName <$> (arbitrary :: Gen PossibleVariableName)
        value <- arbitrary :: Gen Value
        pure $ Result variable value

instance Arbitrary Value where
    arbitrary = sized $ \n -> frequency
        [ (n, Const <$> (getPossibleText <$> (arbitrary :: Gen PossibleText)))
        , (1, Tuple <$> generateResults n)
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

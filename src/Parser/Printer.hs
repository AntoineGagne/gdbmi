{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Printer where

import Control.Lens
    ( (^.) )

import Data.Monoid
    ( (<>) )
import qualified Data.Text as T

import Parser.Types

showOutput :: Output -> T.Text
showOutput output' 
    = T.concat (map showOutOfBandRecord (output'^.outOfBandRecord))
    <> maybe "" showResultRecord (output'^.resultRecord)
    <> "(gdb)\n"

showResultRecord :: ResultRecord -> T.Text
showResultRecord resultRecord' 
    = showToken (resultRecord'^.token)
    <> "^"
    <> showResultClass (resultRecord'^.resultClass)
    <> T.concat (map ((<>) "," . showResult) (resultRecord'^.results))
    <> "\n"

showOutOfBandRecord :: OutOfBandRecord -> T.Text
showOutOfBandRecord = \case
    OutOfBandAsyncRecord asyncRecord' -> showAsyncRecord asyncRecord'
    OutOfBandStreamRecord streamRecord' -> showStreamRecord streamRecord'

showAsyncRecord :: AsyncRecord -> T.Text
showAsyncRecord = \case
    ExecAsyncOutput token' asyncOutput' -> f "*" token' asyncOutput'
    StatusAsyncOutput token' asyncOutput' -> f "+" token' asyncOutput'
    NotifyAsyncOutput token' asyncOutput' -> f "=" token' asyncOutput'
  where
      f c token' asyncOutput'
          = showToken token' <> c <> showAsyncOutput asyncOutput' <> "\n"

showToken :: Maybe Token -> T.Text
showToken = maybe "" (T.pack . show)

showAsyncOutput :: AsyncOutput -> T.Text
showAsyncOutput asyncOutput
    = showAsyncClass (asyncOutput^.asyncClass) 
    <> T.concat (map ((<>) "," . showResult) (asyncOutput^.asyncResults))

showAsyncClass :: AsyncClass -> T.Text
showAsyncClass = \case
    Stopped -> "stopped"
    ThreadGroupStarted -> "thread-group-started"
    ThreadGroupAdded -> "thread-group-added"
    ThreadCreated -> "thread-created"
    AsyncClassRunning -> "running"
    ThreadExited -> "thread-exited"
    ThreadGroupExited -> "thread-group-exited"
    BreakpointModified -> "breakpoint-modified"
    LibraryLoaded -> "library-loaded"

showResult :: Result -> T.Text
showResult result 
    = result^.variable
    <> "="
    <> showValue (result^.value)

showValue :: Value -> T.Text
showValue = \case
    Const text -> surround '"' '"' text
    Tuple results'
        -> surround '{' '}'
        $ T.intercalate ","
        $ map showResult results'
    VList list -> showList' list

showList' :: List -> T.Text
showList' = \case
    EmptyList -> "[]"
    ValueList values -> f values showValue
    ResultList results'
        -> f results' showResult
  where
      f v g = surround '[' ']' $ T.intercalate "," $ map g v

surround :: Char -> Char -> T.Text -> T.Text
surround left right text = T.snoc (T.cons left text) right

showResultClass :: ResultClass -> T.Text
showResultClass = \case
    Done -> "done"
    Running -> "running"
    Connected -> "connected"
    Error -> "error"
    Exit -> "exit"

showStreamRecord :: StreamRecord -> T.Text
showStreamRecord = \case
    ConsoleStreamOutput text -> "~" <> surround '"' '"' text
    TargetStreamOutput text -> "@" <> surround '"' '"' text
    LogStreamOutput text -> "&" <> surround '"' '"' text

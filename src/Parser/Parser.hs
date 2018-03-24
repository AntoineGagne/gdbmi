{-# LANGUAGE FlexibleContexts #-}

module Parser.Parser where

import Control.Applicative
    ( (<*) )
import Control.Applicative.Combinators
    ( (<|>)
    , many
    )
import Text.Parsec
    ( try )
import Text.Parsec.Char
    ( digit
    , string
    , char
    , letter
    , oneOf
    , noneOf
    , anyChar
    , endOfLine
    )
import Text.Parsec.Combinator
    ( many1
    , choice
    , optionMaybe
    , between
    , sepBy1
    , sepBy
    , notFollowedBy
    )
import Text.Parsec.Text
    ( Parser )

import qualified Data.Text as T

import qualified Parser.Types as Types

output :: Parser Types.Output
output = Types.Output
    <$> many outOfBandRecord
    <*> optionMaybe resultRecord
    <* string "(gdb)"
    <* nl

resultRecord :: Parser Types.ResultRecord
resultRecord = do
    token' <- optionMaybe token
    char '^'
    resultClass' <- resultClass
    results <- many (char ',' >> result) <* nl
    pure $ Types.ResultRecord token' resultClass' results

outOfBandRecord :: Parser Types.OutOfBandRecord
outOfBandRecord 
    = try (Types.OutOfBandAsyncRecord <$> asyncRecord)
   <|> (Types.OutOfBandStreamRecord <$> streamRecord)

asyncRecord :: Parser Types.AsyncRecord
asyncRecord 
    = try (Types.ExecAsyncOutput <$> maybeToken '*' <*> asyncOutput <* nl)
   <|> try (Types.StatusAsyncOutput <$> maybeToken '+' <*> asyncOutput <* nl)
   <|> (Types.NotifyAsyncOutput <$> maybeToken '=' <*> asyncOutput <* nl)
  where
      maybeToken separator = optionMaybe token <* char separator

nl :: Parser String
nl = try (string "\r\n") <|> try (string "\n") <|> string "\r"

asyncOutput :: Parser Types.AsyncOutput
asyncOutput = Types.AsyncOutput <$> asyncClass <*> many (char ',' >> result)

asyncClass :: Parser Types.AsyncClass
asyncClass 
    = try (string "stopped" >> pure Types.Stopped) 
   <|> try (string "thread-group-added" >> pure Types.ThreadGroupAdded)
   <|> try (string "thread-group-started" >> pure Types.ThreadGroupStarted)
   <|> try (string "thread-created" >> pure Types.ThreadCreated)
   <|> try (string "running" >> pure Types.AsyncClassRunning)
   <|> try (string "thread-group-exited" >> pure Types.ThreadGroupExited)
   <|> try (string "thread-exited" >> pure Types.ThreadExited)
   <|> try (string "breakpoint-modified" >> pure Types.BreakpointModified)
   <|> try (string "library-loaded" >> pure Types.LibraryLoaded)
   <|> pure Types.Others

streamRecord :: Parser Types.StreamRecord
streamRecord 
    = try (char '~' >> Types.ConsoleStreamOutput <$> cstring)
   <|> try (char '@' >> Types.TargetStreamOutput <$> cstring)
   <|> (char '&' >> Types.LogStreamOutput <$> cstring)

resultClass :: Parser Types.ResultClass
resultClass 
    = try (string "done" >> pure Types.Done)
   <|> try (string "running" >> pure Types.Running)
   <|> try (string "connected" >> pure Types.Connected)
   <|> try (string "error" >> pure Types.Error)
   <|> (string "exit" >> pure Types.Exit)

result :: Parser Types.Result
result = do
    variable <- many1 $ choice [letter, digit, oneOf "_-"]
    char '='
    Types.Result (T.pack variable) <$> value

value :: Parser Types.Value
value 
    = try (Types.Tuple <$> between (char '{') (char '}') (sepBy result (char ',')))
   <|> try (Types.VList <$> list)
   <|> (Types.Const <$> cstring)

cstring :: Parser T.Text
cstring = T.concat <$> between (char '"') (char '"') (many characters)

characters :: Parser T.Text
characters = (T.singleton <$> nonEscapedCharacters) <|> escapedCharacters

escapedCharacters :: Parser T.Text
escapedCharacters = do
    first <- char '\\'
    second <- oneOf "\\\"0nrvtbf"
    pure $ T.cons first (T.singleton second)

nonEscapedCharacters :: Parser Char
nonEscapedCharacters = noneOf "\\\"\0\n\r\v\t\b\f"

list :: Parser Types.List
list = try resultsList <|> try valuesList <|> emptyList
  where
      resultsList = Types.ResultList <$> listOf result
      valuesList = Types.ValueList <$> listOf value
      emptyList = string "[]" >> pure Types.EmptyList
      listOf parser = between (char '[') (char ']') (sepBy1 parser (char ','))

token :: Parser Types.Token
token = do
    digits <- many1 digit
    pure $ read digits

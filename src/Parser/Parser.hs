{-# LANGUAGE FlexibleContexts #-}

module Parser.Parser where

import Control.Applicative
    ( (<*) )
import Control.Applicative.Combinators
    ( (<|>)
    , many
    )
import Text.Parsec.Char
    ( digit
    , string
    , char
    , letter
    , oneOf
    , noneOf
    , anyChar
    , crlf
    )
import Text.Parsec.Combinator
    ( many1
    , choice
    , optionMaybe
    , between
    , sepBy1
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
outOfBandRecord = choice
    [ Types.OutOfBandAsyncRecord <$> asyncRecord
    , Types.OutOfBandStreamRecord <$> streamRecord
    ]

asyncRecord :: Parser Types.AsyncRecord
asyncRecord = choice
    [ Types.ExecAsyncOutput <$> maybeToken '*' <*> asyncOutput <* nl
    , Types.StatusAsyncOutput <$> maybeToken '+' <*> asyncOutput <* nl
    , Types.NotifyAsyncOutput <$> maybeToken '=' <*> asyncOutput <* nl
    ]
  where
      maybeToken separator = optionMaybe token <* char separator

nl = choice [string "\r", string "\r\n"]

asyncOutput :: Parser Types.AsyncOutput
asyncOutput = Types.AsyncOutput <$> asyncClass <*> many result

asyncClass :: Parser Types.AsyncClass
asyncClass = choice
    [ string "stopped" >> pure Types.Stopped
    , many1 anyChar >> pure Types.Others
    ]

streamRecord :: Parser Types.StreamRecord
streamRecord = choice
    [ char '~' >> Types.ConsoleStreamOutput <$> cstring
    , char '@' >> Types.TargetStreamOutput <$> cstring
    , char '&' >> Types.LogStreamOutput <$> cstring
    ]

resultClass :: Parser Types.ResultClass
resultClass = choice
    [ string "done" >> pure Types.Done
    , string "running" >> pure Types.Running
    , string "connected" >> pure Types.Connected
    , string "error" >> pure Types.Error
    , string "exit" >> pure Types.Exit
    ]

result :: Parser Types.Result
result = do
    variable <- many1 $ letter <|> digit <|> oneOf "_-"
    char '='
    value' <- value
    Types.Result (T.pack variable) <$> value

value :: Parser Types.Value
value = choice
    [ Types.Tuple <$> between (char '[') (char ']') (sepBy1 result (char ','))
    , Types.VList <$> list
    , Types.Const <$> cstring
    ]

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
list = choice [emptyList, resultsList, valuesList]
  where
      resultsList = Types.ResultList <$> listOf result
      valuesList = Types.ValueList <$> listOf value
      emptyList = string "[]" >> pure Types.EmptyList
      listOf parser = between (char '[') (char ']') (sepBy1 parser (char ','))

token :: Parser Types.Token
token = do
    digits <- many1 digit
    pure $ read digits

{-# LANGUAGE FlexibleContexts #-}

module Parser.Parser
    (
    ) where

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
    , anyChar
    )
import Text.Parsec.Combinator
    ( many1
    , choice
    , between
    , sepBy1
    )
import Text.Parsec.Text
    ( Parser )

import qualified Data.Text as T

import qualified Parser.Types as Types

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
    , Types.Const <$> (T.pack <$> between (char '"') (char '"') (many anyChar))
    ]

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

module Parser.Parser
    (
    ) where

import Control.Applicative.Combinators
    ( (<|>)
    )
import Text.Parsec.Char
    ( digit
    , string
    )
import Text.Parsec.Combinator
    ( many1
    , choice
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

token :: Parser Types.Token
token = do
    digits <- many1 digit
    pure $ read digits

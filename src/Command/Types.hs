{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Command.Types
    ( Command
    ) where

import Data.Monoid
    ( (<>) )
import Control.Lens
    ( makeLenses
    , (^.)
    )

import qualified Data.Text as T

import Parser.Printer
    ( showToken )
import Parser.Types
    ( Token )

data Command 
    = CliCommand (Maybe Token) GdbCliCommand
    | MiCommand (Maybe Token) Operation [Option] [Parameter]
    deriving (Show)

newtype GdbCliCommand
    = GdbCliCommand
    { _command :: T.Text }
    deriving (Show)

newtype Operation
    = Operation
    { _operation :: T.Text }
    deriving (Show)

data Option = Option 
    { _mandatory :: Parameter
    , _others :: [Parameter]
    } deriving (Show)

data Parameter
    = QuotedParameter T.Text
    | RawParameter T.Text
    deriving (Show)

makeLenses ''GdbCliCommand
makeLenses ''Operation
makeLenses ''Option

showCommand :: Command -> T.Text
showCommand = \case
    CliCommand token command' -> showToken token <> " " <> showGdbCliCommand command'
    MiCommand token operation' options' parameters'
        -> showToken token <> "-" 
                           <> operation'^.operation
                           <> T.concat (map ((<>) " " . showOption) options')
                           <> " --"
                           <> T.concat (map ((<>) " " . showParameter) parameters')

showGdbCliCommand :: GdbCliCommand -> T.Text
showGdbCliCommand command' = command'^.command <> "\n"

showOption :: Option -> T.Text
showOption option' 
    = "-" 
    <> showParameter (option'^.mandatory)
    <> T.concat (map ((<>) " " . showParameter) (option'^.others))

showParameter :: Parameter -> T.Text
showParameter = \case
    QuotedParameter parameter' -> "\"" <> parameter' <> "\""
    RawParameter parameter' -> parameter'

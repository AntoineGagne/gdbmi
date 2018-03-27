{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Command.Types
    ( Command
    , token
    , breakAfter
    , showCommand
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

data Command = Command
    { _token :: Maybe Token
    , _command :: GdbCommand
    } deriving (Show)

data GdbCommand
    = CliCommand GdbCliCommand
    | MiCommand Operation [Option] [Parameter]
    deriving (Show)

newtype GdbCliCommand
    = GdbCliCommand
    { _cliCommand :: T.Text }
    deriving (Show)

data Operation
    = Operation
    { _operation :: T.Text
    , _hasDashes :: Bool
    } deriving (Show)

data Option = Option 
    { _optionName :: Parameter
    , _arguments :: [Parameter]
    } deriving (Show)

data Parameter
    = QuotedParameter T.Text
    | RawParameter T.Text
    deriving (Show)

makeLenses ''Command
makeLenses ''GdbCliCommand
makeLenses ''Operation
makeLenses ''Option

breakAfter :: Integer -> Integer -> Command
breakAfter number count 
    = Command Nothing 
    $ MiCommand
        (Operation "break-after" False)
        [] (map (RawParameter . T.pack . show) [number, count])

showCommand :: Command -> T.Text
showCommand command' = showToken (command'^.token) 
                    <> showGdbCommand (command'^.command)

showGdbCommand :: GdbCommand -> T.Text
showGdbCommand = \case
    CliCommand command' -> showGdbCliCommand command'
    MiCommand operation' options' parameters'
        -> "-" <> operation'^.operation
               <> T.concat (map ((<>) " " . showOption) options')
               <> if operation'^.hasDashes then " --" else ""
               <> T.concat (map ((<>) " " . showParameter) parameters')
  where
    showGdbCliCommand :: GdbCliCommand -> T.Text
    showGdbCliCommand command' = command'^.cliCommand <> "\n"
    showOption :: Option -> T.Text
    showOption option' 
        = "-" 
        <> showParameter (option'^.optionName)
        <> T.concat (map ((<>) " " . showParameter) (option'^.arguments))
    showParameter :: Parameter -> T.Text
    showParameter = \case
        QuotedParameter parameter' -> "\"" <> parameter' <> "\""
        RawParameter parameter' -> parameter'

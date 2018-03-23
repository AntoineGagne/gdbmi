{-# LANGUAGE TemplateHaskell #-}

module Parser.Types where

import Control.Lens
    ( makeLenses
    )
import qualified Data.Text as T

data Result = Result
    { _variable :: T.Text
    , _value :: Value
    } deriving (Show)

data Value
    = Const T.Text
    | Tuple [Result]
    | VList List
    deriving (Show)

data List
    = EmptyList
    | ValueList [Value]
    | ResultList [Result]
    deriving (Show)

data ResultClass
    = Done
    | Running
    | Connected
    | Error
    | Exit
    deriving (Show)

data StreamRecord
    = ConsoleStreamOutput T.Text
    | TargetStreamOutput T.Text
    | LogStreamOutput T.Text
    deriving (Show)

$(makeLenses ''Result)

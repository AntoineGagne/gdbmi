{-# LANGUAGE TemplateHaskell #-}

module Parser.Types where

import Control.Lens
    ( makeLenses
    )
import qualified Data.Text as T

data Output = Output
    { _outOfBandRecord :: [OutOfBandRecord]
    , _resultRecord :: Maybe ResultRecord
    } deriving (Show)

data ResultRecord = ResultRecord
    { _token :: Maybe Token
    , _resultClass :: ResultClass
    , _results :: [Result]
    } deriving (Show)

data OutOfBandRecord
    = OutOfBandAsyncRecord AsyncRecord
    | OutOfBandStreamRecord StreamRecord
    deriving (Show)

data AsyncRecord
    = ExecAsyncOutput (Maybe Token) AsyncOutput
    | StatusAsyncOutput (Maybe Token) AsyncOutput
    | NotifyAsyncOutput (Maybe Token) AsyncOutput
    deriving (Show)

data AsyncOutput = AsyncOutput
    { _asyncClass :: AsyncClass
    , _asyncResults :: [Result]
    } deriving (Show)

type Token = Integer

data AsyncClass
    = Stopped
    | ThreadGroupAdded
    | ThreadGroupStarted
    | ThreadCreated
    | AsyncClassRunning
    | ThreadExited
    | ThreadGroupExited
    | BreakpointModified
    | LibraryLoaded
    deriving (Show)

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

makeLenses ''Result
makeLenses ''AsyncOutput
makeLenses ''ResultRecord
makeLenses ''Output

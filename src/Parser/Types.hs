{-# LANGUAGE TemplateHaskell #-}

module Parser.Types where

import Control.Lens
    ( makeLenses
    )
import qualified Data.Text as T

data Output = Output
    { _outOfBandRecord :: [OutOfBandRecord]
    , _resultRecord :: Maybe ResultRecord
    } deriving (Eq, Show)

data ResultRecord = ResultRecord
    { _token :: Maybe Token
    , _resultClass :: ResultClass
    , _results :: [Result]
    } deriving (Eq, Show)

data OutOfBandRecord
    = OutOfBandAsyncRecord AsyncRecord
    | OutOfBandStreamRecord StreamRecord
    deriving (Eq, Show)

data AsyncRecord
    = ExecAsyncOutput (Maybe Token) AsyncOutput
    | StatusAsyncOutput (Maybe Token) AsyncOutput
    | NotifyAsyncOutput (Maybe Token) AsyncOutput
    deriving (Eq, Show)

data AsyncOutput = AsyncOutput
    { _asyncClass :: AsyncClass
    , _asyncResults :: [Result]
    } deriving (Eq, Show)

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
    deriving (Eq, Show)

data Result = Result
    { _variable :: T.Text
    , _value :: Value
    } deriving (Eq, Show)

data Value
    = Const T.Text
    | Tuple [Result]
    | VList List
    deriving (Eq, Show)

data List
    = EmptyList
    | ValueList [Value]
    | ResultList [Result]
    deriving (Eq, Show)

data ResultClass
    = Done
    | Running
    | Connected
    | Error
    | Exit
    deriving (Eq, Show)

data StreamRecord
    = ConsoleStreamOutput T.Text
    | TargetStreamOutput T.Text
    | LogStreamOutput T.Text
    deriving (Eq, Show)

makeLenses ''Result
makeLenses ''AsyncOutput
makeLenses ''ResultRecord
makeLenses ''Output

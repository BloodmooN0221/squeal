module Time where

newtype Seconds = Seconds { getSeconds :: Int } deriving Show

newtype MicroSeconds = MicroSeconds { getMicro :: Int } deriving Show

secondsToMicro :: Seconds -> MicroSeconds
secondsToMicro seconds = MicroSeconds $ (getSeconds seconds) * 1000000

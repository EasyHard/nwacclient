module Lib
    ( 
        libFunc
    ) where

import qualified Data.ByteString.Lazy.Char8 as B

import Destination
import Mics
import CmdOptions

data Env = Env {
    destinationOption :: DestinationOption 
    , timeRange :: TimeRange
}

getEnv :: IO Env
getEnv = do
    cmdOpt <- getCmdOptions
    return $ Env (CmdOptions.destination cmdOpt) (CmdOptions.timeRange cmdOpt) 

printSnowInfo :: SnowInfo -> IO ()
printSnowInfo snowInfo = B.putStrLn $ prettyJsonSnowInfo snowInfo

libFunc :: IO ()
libFunc = do
    env <- getEnv
    snowInfo <- fetchSnowInfo (destinationOption env) (Lib.timeRange env)
    printSnowInfo snowInfo
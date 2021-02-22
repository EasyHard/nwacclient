module Lib
    ( 
        libFunc
    ) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (sortOn)

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

printSnowInfo :: [SnowInfo] -> IO ()
printSnowInfo snowInfo = B.putStrLn $ prettyJsonSnowInfo snowInfo

getDestinationList :: DestinationOption -> [Destination]
getDestinationList destOpt = 
    case destOpt of
        Dest x -> [x]
        AllDest -> [Destination.SnoqualmiePass, Destination.StevensPass, Destination.Crystal, Destination.MtBaker, Destination.MissionRidge]

libFunc :: IO ()
libFunc = do
    env <- getEnv
    snowInfo <- mapM (fetchSnowInfo $ Lib.timeRange env) (getDestinationList . destinationOption $ env)
    let sortedSnowInfo = reverse $ sortOn snowfall snowInfo
    printSnowInfo sortedSnowInfo
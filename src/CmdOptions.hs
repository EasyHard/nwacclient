module CmdOptions (
  CmdOptions (..)
  , getCmdOptions
  , DestinationOption (..)
) where

import Data.Monoid
import Options.Applicative

import Destination
import Mics

type DestinationOption = Destination.Destination 

data CmdOptions = CmdOptions { 
  destination :: DestinationOption 
  , timeRange :: TimeRange
}

options :: Parser CmdOptions
options = CmdOptions <$> destination <*> timeRange
  where 
    destination :: Parser DestinationOption
    destination = option auto (
      long "dest"
      <> short 'd'
      <> help "Destination to get snow info." )

    timeRange :: Parser TimeRange
    timeRange = PastHour <$> 
      option auto
        ( long "pasthour"
        <> short 'n'
        <> metavar "PastHour"
        <> help "How many past hour to fetch for snow info" )

opts :: ParserInfo CmdOptions
opts = info (helper <*> options) fullDesc

getCmdOptions :: IO CmdOptions
getCmdOptions = execParser opts
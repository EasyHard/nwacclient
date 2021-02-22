module CmdOptions (
  CmdOptions (..)
  , getCmdOptions
  , DestinationOption (..)
) where

import Data.Monoid
import Options.Applicative

import Destination
import Mics

data DestinationOption = Dest Destination.Destination | AllDest deriving (Read)

data CmdOptions = CmdOptions { 
  destination :: DestinationOption 
  , timeRange :: TimeRange
}

isAllDest :: Bool -> DestinationOption
isAllDest True = AllDest
isAllDest False = error "not right"

options :: Parser CmdOptions
options = CmdOptions <$> destination <*> timeRange
  where 
    destination :: Parser DestinationOption
    destination = destOpt1 <|> destOpt2

    destOpt2 :: Parser DestinationOption
    destOpt2 = isAllDest <$> switch
      ( long "all-dest"
        <> help "Fetch snow info for all destinations" )

    destOpt1 :: Parser DestinationOption
    destOpt1 = Dest <$> option auto (
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
{-# LANGUAGE DeriveGeneric #-}

module Destination
    ( 
        Destination (..)
        , SnowInfo (..)
        , fetchSnowInfo
        , prettyJsonSnowInfo
    ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Generics
import Data.ByteString.Lazy (ByteString)
import Debug.Trace

import Mics
import NwacApi
import Measurement

data Destination = SnoqualmiePass | StevensPass | Crystal | MtBaker | Timberline | MissionRidge deriving (Read, Show, Generic)

data SnowInfo = SnowInfo {
    destination :: Destination
    , maxTemperature :: Double
    , minTemperature :: Double
    , snowfall :: Double
    , precipitation :: Double
    , swe :: Double
} deriving (Generic, Show)

instance ToJSON Destination
instance ToJSON SnowInfo 

prettyJsonSnowInfo :: ToJSON a => a -> ByteString
prettyJsonSnowInfo = encodePretty

getDataLogger :: Destination -> DataLogger
getDataLogger x = 
    case x of
        Destination.SnoqualmiePass -> NwacApi.SnoqualmiePass
        Destination.StevensPass -> NwacApi.StevensPass
        Destination.Crystal -> NwacApi.CrystalBase
        Destination.MtBaker -> NwacApi.MtBaker
        Destination.Timberline -> NwacApi.TimberlineLodge
        Destination.MissionRidge -> NwacApi.MissionRidgeMidMountain

createSnowInfo:: Destination -> TimeRange -> SortedMeasurementSeq -> SnowInfo
createSnowInfo destination timeRange seq =
    SnowInfo {
        destination = destination,
        maxTemperature = highestTemperature seq,
        minTemperature = lowestTemperature seq,
        snowfall = totalSnowfall seq,
        precipitation = totalPrecipitation seq,
        swe = (totalSnowfall seq) / (totalPrecipitation seq)
    }

fetchSnowInfo :: TimeRange -> Destination -> Nwac SnowInfo
fetchSnowInfo timeRange destination = do
    seq <- createSortedMeasurementSeq <$> fetchMeasurement (getDataLogger destination) timeRange
    return $ createSnowInfo destination timeRange seq

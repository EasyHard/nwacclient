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

import Mics
import NwacApi
import Measurement

data Destination = SnoqualmiePass | StevensPass | Crystal | MtBaker deriving (Read, Show, Generic)

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

prettyJsonSnowInfo :: SnowInfo -> ByteString
prettyJsonSnowInfo snowInfo = encodePretty snowInfo

getDataLogger :: Destination -> DataLogger
getDataLogger x = 
    case x of
        Destination.SnoqualmiePass -> NwacApi.SnoqualmiePass
        Destination.StevensPass -> NwacApi.StevensPass
        Destination.Crystal -> NwacApi.CrystalBase
        Destination.MtBaker -> NwacApi.MtBaker

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

fetchSnowInfo :: Destination -> TimeRange -> Nwac SnowInfo
fetchSnowInfo destination timeRange = do
    seq <- createSortedMeasurementSeq <$> fetchMeasurement (getDataLogger destination) timeRange
    return $ createSnowInfo destination timeRange seq

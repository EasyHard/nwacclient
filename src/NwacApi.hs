{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module NwacApi
    ( 
        fetchMeasurement
        , fetchMeasurementInternal
        , DataLogger(..)
    ) where

import GHC.Generics
import Data.List.NonEmpty (NonEmpty)
import Data.String (fromString)
import Control.Lens
import Data.Time
import Data.Time.Format
import Data.Time.Format.ISO8601
import Network.Wreq
import Data.Aeson

import Measurement (Measurement)
import Mics
import Text.Printf

import Debug.Trace
import DataLogger

data NwacApiResult = NwacApiResult {
    results :: NonEmpty Measurement
    , count :: Int
} deriving (Generic, Show)

instance FromJSON NwacApiResult

t :: TimeRange -> Nwac (UTCTime, Int)
t (PastHour i) = do
    currTime <- getCurrentTime
    return (currTime, i)
t (Before time i) = do
    return (time, i+1)

fetchMeasurement :: DataLogger -> TimeRange -> Nwac (NonEmpty Measurement)
fetchMeasurement dl tr = do
    (maxDateTime, limit) <- t tr
    fetchMeasurementInternal dl maxDateTime limit

fetchMeasurementInternal :: DataLogger -> UTCTime -> Int -> Nwac (NonEmpty Measurement)
fetchMeasurementInternal dl maxDateTime limit = do
    let loggerId = getDataLoggerId dl
        maxDateTimeStr = formatTime defaultTimeLocale "%Y-%m-%dT%H:00:00Z" maxDateTime 
        opts = defaults & param "data_logger" .~ [fromString . show $ loggerId]
                        & param "limit" .~ [fromString . show $ limit]
                        & param "max_datetime" .~ [fromString maxDateTimeStr]
    -- traceM $ show opts
    -- traceM $ printf "limit = %d, max_datetime = %s" limit maxDateTimeStr
    r <- asJSON =<< getWith opts "https://nwac.us/api/v5/measurement"
    -- traceM $ show (r ^. responseBody)
    return . results $ r ^. responseBody

getDataLoggerId :: DataLogger -> Int
getDataLoggerId dataLogger = 
    case dataLogger of
        SnoqualmiePass -> 21
        StevensPass -> 13
        MtBaker -> 5
        CrystalBase -> 28
        CrystalGreenValley -> 27
        SnoqualmiePassDodgeRidge -> 22
        TimberlineLodge -> 44
        MissionRidgeMidMountain -> 26
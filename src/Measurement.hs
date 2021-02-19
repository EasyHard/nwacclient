{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Measurement
    ( 
        Measurement
        , SortedMeasurementSeq
        , createSortedMeasurementSeq
        , totalPrecipitation
        , highestTemperature
        , lowestTemperature
        , totalSnowfall
    ) where

import GHC.Generics
import Data.Time
import Data.Aeson
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty, nonEmpty, map, scanr1, last, zip, head, take, length, sortWith, NonEmpty( (:|) ))
import Data.Maybe (fromJust)

data Measurement = Measurement {
    datetime :: UTCTime
    , snow_depth :: Maybe Double
    , precipitation :: Maybe Double
    , snowfall_24_hour :: Maybe Double
    , temperature :: Maybe Double
    , wind_speed_average :: Maybe Double
    , wind_speed_maximum :: Maybe Double
    , wind_speed_minimum :: Maybe Double
} deriving (Generic, Show)

instance FromJSON Measurement

data SortedMeasurementSeq = SMS (NonEmpty Measurement)

createSortedMeasurementSeq :: NonEmpty Measurement -> SortedMeasurementSeq 
createSortedMeasurementSeq measurements = SMS $ sortWith datetime measurements

assume :: (Measurement -> Maybe Double) -> SortedMeasurementSeq -> NonEmpty Double
assume f (SMS x) = (fromJust . f) `Data.List.NonEmpty.map` x

sFoldr1 :: (a-> a-> a) -> NonEmpty a -> a
sFoldr1 f x = Data.List.NonEmpty.last $ Data.List.NonEmpty.scanr1 f x

sMax :: Ord a => NonEmpty a -> a
sMax = sFoldr1 max

sSum = sFoldr1 (+)

sMin :: Ord a => NonEmpty a -> a
sMin = sFoldr1 min

totalPrecipitation :: SortedMeasurementSeq -> Double
totalPrecipitation = sSum . assume precipitation

highestTemperature :: SortedMeasurementSeq -> Double
highestTemperature = sMax . assume temperature

lowestTemperature :: SortedMeasurementSeq -> Double
lowestTemperature = sMin .assume temperature

-- This one is a little tricky for two reasons
-- 1) accumulation got cleared in different time for
-- different data logger and 
-- 2) the reading could peak to strange value from
-- time to time.
totalSnowfall :: SortedMeasurementSeq -> Double
totalSnowfall x = 
    let snowfall = assume snowfall_24_hour x
        prevSnowfall = Data.List.NonEmpty.head snowfall :| Data.List.NonEmpty.take ((Data.List.NonEmpty.length snowfall) - 1) snowfall
        p = Data.List.NonEmpty.zip prevSnowfall snowfall
    in
        sSum $ Data.List.NonEmpty.map getAccumeratedSnowfall p

getAccumeratedSnowfall :: (Double, Double) -> Double
getAccumeratedSnowfall (prev, curr)
    | curr - prev >= 4 = 0
    | curr <= 0 = 0
    | curr - prev > 0 = curr - prev
    | curr < prev && curr < 3.5 = curr
    | otherwise = 0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

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

import Debug.Trace

import DataLogger

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

data SortedMeasurementSeq = SMS DataLogger (NonEmpty Measurement) deriving (Show)

createSortedMeasurementSeq :: DataLogger -> NonEmpty Measurement -> SortedMeasurementSeq 
createSortedMeasurementSeq dataLogger measurements = SMS dataLogger (sortWith datetime measurements)

dataLogger :: SortedMeasurementSeq -> DataLogger
dataLogger (SMS dl _) = dl

assume :: (Measurement -> Maybe Double) -> SortedMeasurementSeq -> NonEmpty Double
assume f (SMS _ x) = (fromJust . f) `Data.List.NonEmpty.map` x

sFoldr1 :: (a-> a-> a) -> NonEmpty a -> a
sFoldr1 f x = Data.List.NonEmpty.head $ Data.List.NonEmpty.scanr1 f x

sMax :: Ord a => NonEmpty a -> a
sMax = sFoldr1 max

sSum = sFoldr1 (+)

sMin :: Ord a => NonEmpty a -> a
sMin = sFoldr1 min

filterPrecipitation :: NonEmpty Double -> NonEmpty Double
filterPrecipitation l = Data.List.NonEmpty.map f l
    where f = \x -> if
                | x >= 0.7 -> 0
                | otherwise -> x 

totalPrecipitation :: SortedMeasurementSeq -> Double
totalPrecipitation seq = (sSum l) -- - (Data.List.NonEmpty.head l)
    where 
        l = filterPrecipitation $ assume precipitation seq

highestTemperature :: SortedMeasurementSeq -> Double
highestTemperature = sMax . assume temperature

lowestTemperature :: SortedMeasurementSeq -> Double
lowestTemperature = sMin .assume temperature

shiftOne :: NonEmpty a -> NonEmpty (a, a)
shiftOne l = Data.List.NonEmpty.zip prev l
    where prev = Data.List.NonEmpty.head l :| Data.List.NonEmpty.take ((Data.List.NonEmpty.length l) - 1) l
    
resetBump :: (Double, Double) -> (Double, Double)
resetBump (prev, curr)
    | curr - prev >= 4 = (prev, prev)
    | curr - prev <= 0 && curr - prev > -2 = (prev, prev)
    | otherwise = (prev, curr)

eraseBump :: NonEmpty Double -> NonEmpty Double
eraseBump l = Data.List.NonEmpty.map snd s
    where s = Data.List.NonEmpty.map resetBump $ shiftOne l

adjust :: DataLogger -> NonEmpty Double -> NonEmpty Double
adjust dl snowfall =
    case dl of
        MissionRidgeMidMountain -> Data.List.NonEmpty.map (1-) snowfall
        otherwise -> snowfall

-- This one is a little tricky for two reasons
-- 1) accumulation got cleared in different time for
-- different data logger and 
-- 2) the reading could peak to strange value from
-- time to time.
totalSnowfall :: SortedMeasurementSeq -> Double
totalSnowfall x = 
    let 
        rawSnowfall = assume snowfall_24_hour x
        snowfall = eraseBump . eraseBump . eraseBump $ adjust (dataLogger x) rawSnowfall
        p = shiftOne snowfall
    in
        -- trace (show rawSnowfall)
        -- trace (show snowfall)
        -- trace (show p)
        sSum $ Data.List.NonEmpty.map getAccumeratedSnowfall p

getAccumeratedSnowfall :: (Double, Double) -> Double
getAccumeratedSnowfall (prev, curr)
    | curr - prev >= 4 = 0
    | curr <= 0 = 0
    | curr - prev > 0 = curr - prev
    | curr < prev && curr - prev < -2 && curr < 3.5 && curr > 0 = curr
    | otherwise = 0
module Mics ( 
    TimeRange(..)
    , Nwac
) where

import Data.Time

data TimeRange = PastHour Int | Before UTCTime Int
type Nwac = IO

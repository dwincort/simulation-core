{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- | An implementation of time which is just an 'Int' representing the number of
-- seconds since UTC 0 (the beginning of 1970).
--
-- Note that this implementation has precision only up to seconds and truncates
-- beyond that.
module Simulation.Time where

import Prelude

import Control.DeepSeq (NFData)

import Data.Aeson (FromJSON(..), FromJSONKey(..), FromJSONKeyFunction(..), ToJSON(..),
                   ToJSONKey(..), ToJSONKeyFunction(..))

import Data.Time
import Data.Time.Clock.POSIX
import Data.Vector.Unboxed.Deriving

import GHC.Generics

-- | Number of seconds since UTC 0
newtype Time = Time Int
  deriving (Eq, Ord, Enum, Show, NFData, Generic)

-- | Number of seconds
newtype DeltaTime = DeltaTime Int
  deriving (Eq, Ord, Enum, Show, Num, NFData, Generic)

instance ToJSON Time where
  toJSON = toJSON . humanReadableSimTime

instance FromJSON Time where
  parseJSON = fmap toTime . parseJSON
    where toTime (day, timeOfDay) = timeOfDayToSimTime timeOfDay .+ dayToSimTime day

instance ToJSONKey Time where
  toJSONKey = ToJSONKeyValue toJSON toEncoding

instance FromJSONKey Time where
  fromJSONKey = FromJSONKeyValue parseJSON

instance ToJSON DeltaTime where
  toJSON = toJSON . deltaTimeInSeconds

instance FromJSON DeltaTime where
  parseJSON = fmap toDeltaTime . parseJSON
    where toDeltaTime = timeOfDayToSimTime . TimeOfDay 0 0

instance ToJSONKey DeltaTime where
  toJSONKey = ToJSONKeyValue toJSON toEncoding

instance FromJSONKey DeltaTime where
  fromJSONKey = FromJSONKeyValue parseJSON

-- | Adds an amount to an absolute time.
infixr 6 .+
(.+) :: DeltaTime -> Time -> Time
DeltaTime x .+ Time y = Time $ x + y
{-# INLINE (.+) #-}

-- | Finds the amount of time difference between two absolute times.
infixr 6 .-
(.-) :: Time -> Time -> DeltaTime
Time x .- Time y = DeltaTime $ x - y
{-# INLINE (.-) #-}

-- | Convert from 'TimeOfDay' to the simulator's notion of time.
-- NOTE: We round seconds down to the nearest whole number.
timeOfDayToSimTime :: TimeOfDay -> DeltaTime
timeOfDayToSimTime (TimeOfDay h m s) = DeltaTime $
  ((fromIntegral h) * 60 + (fromIntegral m)) * 60 + (floor s)

-- | Convert from 'Day' to the simulator's notion of time.
-- This should return the absolute time at midnight at the start of the given day.
--
-- NOTE: If we start dealing with time zones, we'll want to modify this.
dayToSimTime :: Day -> Time
dayToSimTime d = Time $ floor $ utcTimeToPOSIXSeconds $ UTCTime d 0

-- | In the future, we may want to use 'ZonedTime' or in some way incorporate
-- time zones.
localTimeToSimTime :: LocalTime -> Time
localTimeToSimTime LocalTime{..} =
  timeOfDayToSimTime localTimeOfDay .+ dayToSimTime localDay


-- | Converts from 'DiffTime' to 'DeltaTime'
diffTimeToSimTime :: DiffTime -> DeltaTime
diffTimeToSimTime = DeltaTime . floor . toRational
{-# INLINE diffTimeToSimTime #-}

-- | Convert the internal simulation time to something human readable
--
-- NOTE: If we start dealing with time zones, we'll want to modify this.
humanReadableSimTime :: Time -> (Day, TimeOfDay)
humanReadableSimTime (Time t) = (day, timeToTimeOfDay diffTime)
  where UTCTime day diffTime = posixSecondsToUTCTime (fromIntegral t)
{-# INLINE humanReadableSimTime #-}

-- | Interpret the given delta time as a number of seconds.
deltaTimeInSeconds :: DeltaTime -> Int
deltaTimeInSeconds (DeltaTime t) = t
{-# INLINE deltaTimeInSeconds #-}

-- | Given a numeric value, scale the given time by that value.
scaleDeltaTime :: Double -> DeltaTime -> DeltaTime
scaleDeltaTime scalar (DeltaTime dt) = DeltaTime $ floor $ scalar * fromIntegral dt
{-# INLINE scaleDeltaTime #-}

-- | The length of one day
oneDay :: DeltaTime
oneDay = DeltaTime 86400
{-# INLINE oneDay #-}

-- | The length of one hour
oneHour :: DeltaTime
oneHour = DeltaTime 3600
{-# INLINE oneHour #-}

-- | The length of one minute
oneMinute :: DeltaTime
oneMinute = DeltaTime 60
{-# INLINE oneMinute #-}

-- | The length of one second
oneSecond :: DeltaTime
oneSecond = DeltaTime 1
{-# INLINE oneSecond #-}

derivingUnbox "DeltaTime"
    [t| DeltaTime -> Int |]
    [| deltaTimeInSeconds |]
    [| DeltaTime |]

{-# LANGUAGE DeriveAnyClass #-}
-- | Simulation
module Simulation.Simulation.PriorityQueue
  ( Event(..)
  , Simulation(..)
  , simulationFromList
  , singleEventSimulation
  , nextSimEvent
  )
where

import Prelude

import Control.DeepSeq (NFData)

import           Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ

import GHC.Generics (Generic)

import Simulation.Time


-- * Events

-- | A value at a given time.
data Event a = Event
  { time  :: !Time
  , value :: a
  } deriving (Show, Eq, Generic, Functor, NFData)

-- * Simulation

-- | A simulation is a queue of 'ScheduleT v m' actions.
newtype Simulation a = Simulation (MinPQueue Time a)

instance Semigroup (Simulation a) where
  (Simulation m1) <> (Simulation m2) = Simulation $ PQ.union m1 m2

instance Monoid (Simulation a) where
    mempty  = Simulation PQ.empty
    mappend = (<>)

-- | Construct a simulation from a list of events.
simulationFromList :: [Event a] -> Simulation a
simulationFromList lst = Simulation $
  PQ.fromList [(t, a) | Event t a <- lst]
{-# INLINABLE simulationFromList #-}

singleEventSimulation :: Event a -> Simulation a
singleEventSimulation Event{..} = Simulation $ PQ.singleton time value
{-# INLINABLE singleEventSimulation #-}

-- | Get the next event from the 'Simulation'.
nextSimEvent :: Simulation a -> Maybe ((Time, a), Simulation a)
nextSimEvent (Simulation s) = fmap Simulation <$> PQ.minViewWithKey s
{-# INLINABLE nextSimEvent #-}

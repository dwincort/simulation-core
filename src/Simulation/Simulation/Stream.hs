{-# LANGUAGE DeriveAnyClass #-}
-- | Simulation
module Simulation.Simulation.Stream
  ( Event(..)
  , Simulation(..)
  , simulationFromList
  , singleEventSimulation
  , nextSimEvent
  )
where

import Tgt.Prelude

import Control.DeepSeq (NFData)

import GHC.Generics (Generic)

import           Simulation.Streaming (Stream)
import qualified Simulation.Streaming as S

import Simulation.Time


-- * Events

-- | A value at a given time.
data Event a = Event
  { time  :: !Time
  , value :: a
  } deriving (Show, Eq, Generic, Functor, NFData)

-- * Simulation

-- | A simulation is a queue of 'ScheduleT v m' actions.
newtype Simulation a = Simulation (Stream Identity (Event a))

instance Semigroup (Simulation a) where
  (Simulation m1) <> (Simulation m2) = Simulation $ S.mergeOn time m1 m2

instance Monoid (Simulation a) where
    mempty  = Simulation mempty
    mappend = (<>)

-- | Construct a simulation from a sorted list of events.
simulationFromList :: [Event a] -> Simulation a
simulationFromList = Simulation . S.fromList
{-# INLINABLE simulationFromList #-}

singleEventSimulation :: Event a -> Simulation a
singleEventSimulation e = Simulation $ S.fromList [e]
{-# INLINABLE singleEventSimulation #-}

-- | Get the next event from the 'Simulation'.
nextSimEvent :: Simulation a -> Maybe ((Time, a), Simulation a)
nextSimEvent (Simulation s) = fmap (\(Event{..}, s') -> ((time, value), Simulation s'))
  $ runIdentity $ S.uncons s
{-# INLINABLE nextSimEvent #-}

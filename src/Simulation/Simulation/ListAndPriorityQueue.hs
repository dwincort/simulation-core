{-# LANGUAGE DeriveAnyClass #-}
-- | Simulation
module Simulation.Simulation.ListAndPriorityQueue
  ( Event(..)
  , Simulation(..)
  , simulationFromList
  , singleEventSimulation
  , nextSimEvent
  )
where

import Tgt.Prelude

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
data Simulation a = Simulation
  { preScheduled :: ![(Event a)]
  , scheduled    :: !(MinPQueue Time a)
  }

instance Semigroup (Simulation a) where
  (Simulation pre1 s1) <> (Simulation pre2 s2) =
    Simulation (mergeOn time pre1 pre2) (PQ.union s1 s2)

instance Monoid (Simulation a) where
    mempty  = Simulation [] PQ.empty
    mappend = (<>)

-- | Construct a simulation from a sorted list of events.
simulationFromList :: [Event a] -> Simulation a
simulationFromList lst = Simulation lst PQ.empty
{-# INLINABLE simulationFromList #-}

singleEventSimulation :: Event a -> Simulation a
singleEventSimulation Event{..} = Simulation [] $ PQ.singleton time value
{-# INLINABLE singleEventSimulation #-}

-- | Get the next event from the 'Simulation'.
nextSimEvent :: Simulation a -> Maybe ((Time, a), Simulation a)
nextSimEvent (Simulation [] pq) = fmap (Simulation []) <$> PQ.minViewWithKey pq
nextSimEvent (Simulation (e@(Event t1 v1) : es) pq) = case PQ.getMin pq of
  Nothing       -> Just ((t1, v1), Simulation es pq)
  Just (t2, v2) -> case compare t1 t2 of
    LT -> Just ((t1, v1), Simulation es pq)
    EQ -> Just ((t2, v2), Simulation (e:es) (PQ.deleteMin pq)) -- Biased toward pre-scheduled events
    GT -> Just ((t2, v2), Simulation (e:es) (PQ.deleteMin pq))
{-# INLINABLE nextSimEvent #-}

mergeOn :: Ord b
  => (a -> b)
  -> [a] -> [a] -> [a]
mergeOn = mergeBy . comparing
{-# INLINE mergeOn #-}

{- | Merge two lists, ordering the elements using the given comparison function.
-}
mergeBy ::
     (a -> a -> Ordering)
  -> [a] -> [a] -> [a]
mergeBy cmp = loop
  where
    loop [] xs = xs
    loop xs [] = xs
    loop (x:xs) (y:ys) = case cmp x y of
      LT -> x : loop xs (y:ys)
      EQ -> x : loop xs (y:ys) -- left-biased
      GT -> y : loop (x:xs) ys
{-# INLINABLE mergeBy #-}

{-# LANGUAGE DeriveAnyClass #-}
-- | Simulation
module Simulation.Simulation.List
  ( Event(..)
  , Simulation(..)
  , simulationFromList
  , singleEventSimulation
  , nextSimEvent
  )
where

import Control.DeepSeq (NFData)

import Data.Ord (comparing)

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
newtype Simulation a = Simulation [Event a]

instance Semigroup (Simulation a) where
  (Simulation m1) <> (Simulation m2) = Simulation $ mergeOn time m1 m2

instance Monoid (Simulation a) where
    mempty  = Simulation []
    mappend = (<>)

-- | Construct a simulation from a sorted list of events.
simulationFromList :: [Event a] -> Simulation a
simulationFromList = Simulation
{-# INLINABLE simulationFromList #-}

singleEventSimulation :: Event a -> Simulation a
singleEventSimulation e = Simulation [e]
{-# INLINABLE singleEventSimulation #-}

-- | Get the next event from the 'Simulation'.
nextSimEvent :: Simulation a -> Maybe ((Time, a), Simulation a)
nextSimEvent (Simulation []) = Nothing
nextSimEvent (Simulation ((Event t a):xs)) = Just ((t, a), Simulation xs)
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

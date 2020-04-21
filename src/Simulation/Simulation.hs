{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simulation
module Simulation.Simulation
  ( Handler(..)

  , Observable(..)

  , Event(..)

  , Simulation
  , streamSimulation
  , streamSimulationUntil
  , simulationFromList

  , ScheduleT(..)

  , Action
  , makeAction
  , doAction
  , startingAction
  , performNow
  , scheduleImmediately
  , scheduleAfter
  , scheduleAt
  , currentTime
  , readState

  -- re-exports
  , KnownSymbol
  )
where

import Prelude

import Control.Applicative    (Applicative(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Morph
import Control.Monad.Primitive

import Data.STRef (STRef, readSTRef, writeSTRef)
import Data.Maybe (fromMaybe)
import Data.Proxy

import GHC.TypeLits

import           Simulation.Simulation.List
import           Simulation.Streaming       (Stream)
import qualified Simulation.Streaming       as S

import Simulation.Time


-- * Handlers

-- | A handler is a state transforming function with an additional input and
-- output.  If the state does not need to be changed, 'Nothing' can be output
-- instead of a new state.
--
-- The type variables represent the following things:
--
-- [@name@] The type-level name of the 'Handler'.
--
-- [@m@] The underlying monad (typically, of the form 'ScheduleT v n' for some
--   observable @v@ and other monad @n@).
--
-- [@s@] The state that is being transformed.
--
-- [@i@] Any extra input to the handler.
--
-- [@o@] The output of the handler.
newtype Handler (name :: Symbol) m s i o =
  Handler { runHandler :: i -> s -> m (o, Maybe s) }

-- * Observation

-- | The 'Observable' type class is how we can convert internal simulation values
-- into final representations 'v'.  The other type variables match up with those
-- of 'Handler'.
class Observable name s i o v where
  -- | Given a name, simulation time, state, input, and output, observe a result.
  observe :: Proxy name -> Time -> s -> i -> o -> v


-- | An internal function to step the simulation through one event.
stepSimulation :: Monad m
  => Maybe Time
  -- ^ If this is @Just t@, don't process any events after @t@.
  -> Simulation (ScheduleT v m v)
  -- ^ The simulation to step.
  -> m (Maybe (Event v, Simulation (ScheduleT v m v)))
stepSimulation endTime = go
  where
    withinTime = case endTime of
      Nothing -> const True
      Just t -> (<= t)
    go s = case nextSimEvent s of
      Just ((t, a), stream) | withinTime t -> do
        (scheduled, v) <- runSchedule a t
        pure $ Just (Event t v, scheduled <> stream)
      _ -> pure $ Nothing
{-# INLINABLE stepSimulation #-}

-- | Stream the results of the simulation as a @Stream@.
streamSimulation :: S.Streamable m
  => Simulation (ScheduleT v m v)
  -> Stream m (Event v)
streamSimulation = S.unfoldr (stepSimulation Nothing)
{-# INLINABLE streamSimulation #-}

-- | Stream the results of the simulation as a @Stream@.
-- Don't simulate beyond the given time.
streamSimulationUntil :: S.Streamable m
  => Time -- End time
  -> Simulation (ScheduleT v m v)
  -> Stream m (Event v)
streamSimulationUntil = S.unfoldr . stepSimulation . Just
{-# INLINABLE streamSimulationUntil #-}


-- * Scheduling

-- | A Monad Transformer to schedule events in the future.  It is first a reader
-- monad that contains the current time, and then a Writer monad for a list
-- of events.  This allows scheduling of both relative- and absolute-timed events.
--
-- Note that @ScheduleT v@ is the monad transformer, and @ScheduleT v m@ is the
-- monad.  In practice, you should rarely (if ever) see 'ScheduleT' by itself
-- instead of @ScheduleT v m@.
newtype ScheduleT v m a = ScheduleT { runSchedule :: Time -> m (Simulation (ScheduleT v m v), a) }
  deriving (Functor)

instance Applicative m => Applicative (ScheduleT v m) where
  pure = ScheduleT . pure . pure . pure
  (ScheduleT f) <*> (ScheduleT x) = ScheduleT $ (liftA2 $ liftA2 (<*>)) f x
  liftA2 f (ScheduleT x) (ScheduleT y) = ScheduleT $ (liftA2 $ liftA2 $ liftA2 f) x y

instance Monad m => Monad (ScheduleT v m) where
  x >>= f = ScheduleT $ \t -> do
    (sim1, a) <- runSchedule x t
    (sim2, b) <- runSchedule (f a) t
    pure (sim1 <> sim2, b)

instance (Semigroup a, Applicative m) => Semigroup (ScheduleT v m a) where
  (ScheduleT x) <> (ScheduleT y) = ScheduleT $ (liftA2 $ liftA2 (<>)) x y

instance (Monoid a, Applicative m) => Monoid (ScheduleT v m a) where
  mempty = pure mempty

instance MonadTrans (ScheduleT v) where
  lift = ScheduleT . const . fmap pure

instance MonadIO m => MonadIO (ScheduleT v m) where
  liftIO = ScheduleT . pure . fmap pure . liftIO

instance PrimMonad m => PrimMonad (ScheduleT v m) where
  type PrimState (ScheduleT v m) = PrimState m
  primitive = ScheduleT . pure . fmap pure . primitive


type Action m v i o = i -> ScheduleT v m (o, v)

-- | Given the location of a node, a handler representing the action to run, and
-- the input to the action, create a 'ScheduleT v m' action.
makeAction :: forall name m v s i o.
  (KnownSymbol name, Observable name s i o v, PrimMonad m)
  => Handler name (ScheduleT v m) s i o -- The event
  -> STRef (PrimState m) s -- The node performing this scheduled event
  -> Action m v i o
makeAction handler ref i = do
  s <- stToPrim $ readSTRef ref
  t <- currentTime
  (!o, !s') <- runHandler handler i s
  let !v = observe @name Proxy t (fromMaybe s s') i o
  case s' of
    Just !s' -> stToPrim $ writeSTRef ref s' >> return (o, v)
    Nothing  -> return (o, v)
{-# INLINABLE makeAction #-}

doAction :: forall m v i o.
  PrimMonad m
  => Action m v i o
  -> i               -- The input for the action
  -> ScheduleT v m v
doAction = (fmap snd .)
{-# INLINE doAction #-}

startingAction :: forall name m v s i o.
  (KnownSymbol name, Observable name s i o v, PrimMonad m)
  => Handler name (ScheduleT v m) s i o -- The event
  -> STRef (PrimState m) s -- The node performing this scheduled event
  -> i
  -> ScheduleT v m v
startingAction = (doAction .) . makeAction

-- | Perform the event "now", or in other words, in place and at the current
-- time.  This is a potentially dangerous function, and it should never be used
-- to perform an event for a node that is already performing an event, but it
-- is an efficient way to perform actions on other nodes in the system without
-- needing to fully schedule them.
performNow :: forall m v i o.
  PrimMonad m
  => Action m v i o
  -> i               -- The input for the action
  -> ScheduleT v m o
performNow = (fmap fst .)
{-# INLINE performNow #-}

-- | Schedule an event to happen immediately after this event.  This is the
-- safe version of 'performNow'.
scheduleImmediately ::
  PrimMonad m
  => Action m v i o
  -> i         -- The input for the event
  -> ScheduleT v m ()
scheduleImmediately = scheduleAfter 0
{-# INLINE scheduleImmediately #-}

-- | Schedule an event to occur after a given amount of time.
-- If the given amount of time is negative, it is automatically set to 0.
scheduleAfter ::
  PrimMonad m
  => DeltaTime      -- The event should be scheduled after this much time
  -> Action m v i o
  -> i              -- The input for the event
  -> ScheduleT v m ()
scheduleAfter dt action i = ScheduleT $ \t ->
  pure (singleEventSimulation (Event (max 0 dt .+ t) $ snd <$> action i), ())
{-# INLINE scheduleAfter #-}

-- | Schedule an event to occur at the given time.
-- If the given time is in the past, it is automatically set to now.
scheduleAt ::
  PrimMonad m
  => Time      -- The event should be scheduled at this time
  -> Action m v i o
  -> i         -- The input for the event
  -> ScheduleT v m ()
scheduleAt t' action i = ScheduleT $ \t ->
  pure (singleEventSimulation (Event (max t t') $ snd <$> action i), ())
{-# INLINE scheduleAt #-}

-- | Get the current time.
currentTime :: Monad m => ScheduleT v m Time
currentTime = ScheduleT $ pure . pure
{-# INLINE currentTime #-}

-- | Read the state of a ref.
readState :: PrimMonad m => STRef (PrimState m) s -> ScheduleT v m s
readState = stToPrim . readSTRef
{-# INLINE readState #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A simple arithmetic example that utilizes the simulation-core engine.
module Simulation.Example where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.ST (stToIO)
import Data.STRef
import GHC.Base (RealWorld)
import GHC.TypeLits (symbolVal)
import qualified Streaming.Prelude as S
import Streaming.Prelude (Stream, Of)

import Simulation.Simulation
import Simulation.Time

-- We make an unwrapped version of IORef so it plays nicely with ST stuff.
type IORef = STRef RealWorld

readIORef :: IORef a -> IO a
readIORef = stToIO . readSTRef

writeIORef :: IORef a -> a -> IO ()
writeIORef = (stToIO .) . writeSTRef

newIORef :: a -> IO (IORef a)
newIORef = stToIO . newSTRef

-- | 'MyHandler' is a simple type synonym for the 'Handler' specified to
-- 'Observation' as the observation type and 'IO' for the underlying monad.
type MyHandler name s i o = Handler name (ScheduleT Observation IO) s i o

-- * Events
--
-- The events could very well be functions rather than classes considering that
-- we have only one node type, but for the sake of the example, we write them
-- out as classes.

-- | An event to add an input int to the state.  The output value is the new internal state.
class Plus s where
  plus :: MyHandler "Plus" s Int Int

-- | An event to multiply the state by an input.  The output value is the new internal state.
class Times s where
  times :: MyHandler "Times" s Int Int

-- | An event to exponentiate the state by an input.  The output value is the new internal state.
class Expo s where
  expo :: MyHandler "Expo" s Int Int

-- * Node

-- | The node processes the events.
-- The node contains its internal state and a pointer to itself (for scheduling
-- events for itself in the future).
data Node = Node Int (IORef Node)

instance Plus Node where
  -- Adding to a node is as simple as adding to the internal state.
  plus = Handler $ \b (Node s ref) ->
    let
      s' = s + b
    in
      pure (s', Just $ Node s' ref)

instance Times Node where
  -- Multiplying a node is the process of repeatedly adding it to itself.
  -- This implementation is limited to inputs that are positive.
  times = Handler $ \b (Node s ref) -> do
    -- Add to itself for the next b time steps.
    _ <- sequenceA [scheduleAfter dt (makeAction plus ref) s | dt <- [1..(fromIntegral b-1)]]
    pure (s, Nothing)

instance Expo Node where
  -- Exponentiating a node is the process of repeatedly multiplying it by itself.
  -- This implementation is limited to inputs that are positive.
  expo = Handler $ \b (Node s ref) -> do
    let sdt = fromIntegral s
        bdt = fromIntegral b
    -- Multiply by self b times, each one separated by s time (to account for the
    -- time it takes to do the multiplication).
    _ <- sequenceA [scheduleAfter dt (makeAction times ref) s | dt <- [1,(sdt+1)..((bdt-1)*sdt)]]
    pure (s, Nothing)

-- * Observations

-- | The 'Observation' is the result that we care about observing after each
-- event.  Here, we would like the name of the event (the arithmetic operator),
-- the input to the event (the operand), and the output of the event (the new state).
data Observation = Observation String Int Int
  deriving (Show)

instance KnownSymbol name => Observable name Node Int Int Observation where
  observe name _time _node input output =
    Observation (symbolVal name) input output


-- * Streaming the simulation

-- | A sample stream of observations.
observations :: Stream (Of (Event Observation)) IO ()
observations = do
  -- The node needs a reference to itself, so we must "tie the knot".
  ref <- liftIO $ newIORef (error "too strict!")
  liftIO $ writeIORef ref (Node 0 ref)
  let
    sim =
      simulationFromList
        [ Event
            { time  = Time 1
            , value = startingAction plus ref 3
            }
        , Event
            { time  = Time 5
            , value = startingAction expo ref 3
            }
        ]
  streamSimulation sim

main :: IO ()
main = S.toList_ observations >>= mapM_ (putStrLn . show)
-- >>> main
-- [Event {time = Time 1,  value = Observation "Plus"  3 3}
-- ,Event {time = Time 5,  value = Observation "Expo"  3 3}
-- ,Event {time = Time 6,  value = Observation "Times" 3 3}
-- ,Event {time = Time 7,  value = Observation "Plus"  3 6}
-- ,Event {time = Time 8,  value = Observation "Plus"  3 9}
-- ,Event {time = Time 9,  value = Observation "Times" 3 9}
-- ,Event {time = Time 10, value = Observation "Plus"  9 18}
-- ,Event {time = Time 11, value = Observation "Plus"  9 27}]

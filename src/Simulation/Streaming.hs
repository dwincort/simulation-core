{-# LANGUAGE ConstraintKinds #-}

module Simulation.Streaming
  ( Stream
  , Streamable
  , unfoldr
  , replicateM
  , mapM
  , nub
  , fold
  , foldM
  , scanM
  , toList
  , fromList
  , uncons
  , mergeOn
  , uniqueScan
  )
where

import Prelude hiding (mapM)

import Data.Ord (comparing)

import qualified Streaming.Internal as S
import qualified Streaming.Prelude  as S

type Stream m a = S.Stream (S.Of a) m ()
type Streamable = Monad

unfoldr :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
unfoldr f = S.unfoldr $ fmap (maybe (Left ()) Right) . f
{-# INLINE unfoldr #-}

replicateM :: Monad m => Int -> m a -> Stream m a
replicateM = S.replicateM
{-# INLINE replicateM #-}

mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapM = S.mapM
{-# INLINE mapM #-}

nub :: (Eq a, Monad m) => Stream m a -> Stream m a
nub = loop Nothing
  where
    loop Nothing  stream = case stream of
      S.Return r           -> S.Return r
      S.Effect m           -> S.Effect (fmap (loop Nothing) m)
      S.Step (a S.:> rest) -> loop (Just a) rest
    loop (Just p) stream = case stream of
      S.Return r           -> S.Step (p S.:> S.Return r)
      S.Effect m           -> S.Effect (fmap (loop (Just p)) m)
      S.Step (a S.:> rest)
        | p == a           -> loop (Just a) rest
        | otherwise        -> S.Step (p S.:> loop (Just a) rest)
{-# INLINABLE nub #-}

fold :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream m a -> m b
fold = S.fold_
{-# INLINE fold #-}

foldM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream m a -> m b
foldM = S.foldM_
{-# INLINE foldM #-}

scanM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream m a -> Stream m b
scanM = S.scanM
{-# INLINE scanM #-}

toList :: Monad m => Stream m a -> m [a]
toList = S.toList_
{-# INLINE toList #-}

fromList :: (Foldable f, Monad m) => f a -> Stream m a
fromList = S.each
{-# INLINE fromList #-}

uncons :: Monad m => Stream m a -> m (Maybe (a, Stream m a))
uncons = S.uncons
{-# INLINE uncons #-}

-- | Perform a scanM, but only emit values if they differ from the last thing
-- emitted.  Furthermore, emit as late as possible (so all monadic effects
-- potentially related to the emitted value have been performed).  Use the
-- extraction function _after_ determining a value should be emitted.
--
-- It should be true that:
-- >>> uniqueScan start f extract == mapM extract . nub . scanM f start pure
uniqueScan :: (Monad m, Eq x)
  => m x
  -> (x -> a -> m x)
  -> (x -> m b)
  -> Stream m a
  -> Stream m b
uniqueScan start f extract = go1
  where
    go1 stream = case stream of
      S.Return r           -> S.Return r
      S.Effect m           -> S.Effect (fmap go1 m)
      S.Step (a S.:> rest) -> S.Effect $ do
        x <- start
        x' <- f x a
        pure $ go2 x' rest
    go2 x stream = case stream of
      S.Return r           -> S.Effect $ do
        b <- extract x
        pure $ S.Step (b S.:> S.Return r)
      S.Effect m           -> S.Effect (fmap (go2 x) m)
      S.Step (a S.:> rest) -> S.Effect $ do
        x' <- f x a
        if x == x'
          then pure $ go2 x' rest
          else do
            b <- extract x
            pure $ S.Step (b S.:> go2 x' rest)
{-# INLINABLE uniqueScan #-}

{- | Merge two streams, ordering them by applying the given function to
   each element before comparing.

   The return values of both streams are returned.
-}
mergeOn :: (Monad m, Ord b)
  => (a -> b)
  -> Stream m a
  -> Stream m a
  -> Stream m a
mergeOn f = mergeBy (comparing f)
{-# INLINE mergeOn #-}

{- | Merge two streams, ordering the elements using the given comparison function.

   The return values of both streams are returned.
-}
mergeBy :: Monad m
  => (a -> a -> Ordering)
  -> Stream m a
  -> Stream m a
  -> Stream m a
mergeBy cmp = loop
  where
    loop str0 str1 = case str0 of
      S.Return _            -> str1
      S.Effect m            -> S.Effect $ fmap (flip loop str1) m
      S.Step (a S.:> rest0) -> case str1 of
        S.Return _            -> str0
        S.Effect m            -> S.Effect $ fmap (loop str0) m
        S.Step (b S.:> rest1) -> case cmp a b of
          LT -> S.Step (a S.:> loop rest0 str1)
          EQ -> S.Step (a S.:> loop rest0 str1) -- left-biased
          GT -> S.Step (b S.:> loop str0 rest1)
{-# INLINABLE mergeBy #-}

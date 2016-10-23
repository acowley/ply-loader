{-# LANGUAGE BangPatterns #-}
module PLY.Internal.StrictReplicate where
import Data.Vector.Generic (Vector, fromList)
import Data.Vector.Fusion.Bundle.Monadic (fromStream, toList)
import Data.Vector.Fusion.Bundle.Size (Size(..))
import Data.Vector.Fusion.Stream.Monadic (Stream (..), Step(..))

-- | Yield a 'Stream' of values obtained by performing the monadic
-- action the given number of times. Each value yielded by the monadic
-- action is evaluated to WHNF.
replicateStreamM' :: Monad m => Int -> m a -> Stream m a
{-# INLINE [1] replicateStreamM' #-}
replicateStreamM' n p = Stream step n
  where
    {-# INLINE [0] step #-}
    step i | i <= 0    = return Done
           | otherwise = do { !x <- p; return $ Yield x (i-1) }

-- |Execute the monadic action the given number of times and store the
-- results in a vector. Each value yielded by the monadic action is
-- evaluated to WHNF.
replicateM' :: (Monad m, Vector v a) => Int -> m a -> m (v a)
replicateM' n p = do let s = replicateStreamM' n p
                     xs <- toList (fromStream s (Exact n))
                     return (fromList xs)
{-# INLINE replicateM' #-}

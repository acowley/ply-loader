{-# LANGUAGE BangPatterns #-}
module PLY.Internal.StrictReplicate where
import Data.Vector.Generic (Vector, unstream)
import Data.Vector.Fusion.Stream (unsafeFromList)
import Data.Vector.Fusion.Stream.Monadic (Stream (..), Step(..), size, toList)
import Data.Vector.Fusion.Util (delay_inline)
import Data.Vector.Fusion.Stream.Size (Size(..))

-- | Yield a 'Stream' of values obtained by performing the monadic
-- action the given number of times. Each value yielded by the monadic
-- action is evaluated to WHNF.
replicateStreamM' :: Monad m => Int -> m a -> Stream m a
{-# INLINE [1] replicateStreamM' #-}
-- NOTE: We delay inlining max here because GHC will create a join point for
-- the call to newArray# otherwise which is not really nice.
replicateStreamM' n p = Stream step n (Exact (delay_inline max n 0))
  where
    {-# INLINE [0] step #-}
    step i | i <= 0    = return Done
           | otherwise = do { !x <- p; return $ Yield x (i-1) }

-- |Execute the monadic action the given number of times and store the
-- results in a vector. Each value yielded by the monadic action is
-- evaluated to WHNF.
replicateM' :: (Monad m, Vector v a) => Int -> m a -> m (v a)
replicateM' n p = do let s = replicateStreamM' n p
                     xs <- toList s
                     return . unstream $ unsafeFromList (size s) xs
{-# INLINE replicateM' #-}

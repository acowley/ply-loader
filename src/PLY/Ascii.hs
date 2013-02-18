{-# LANGUAGE ScopedTypeVariables #-}
module PLY.Ascii where
import Control.Applicative
import Data.Attoparsec.Char8
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Linear.V3 (V3(..))

import PLY.Internal.Parsers
import PLY.Internal.StrictReplicate
import PLY.Types

parseASCII :: Element -> Parser (Vector (Vector Scalar))
parseASCII e = replicateM' (elNum e)
                           (skip *> (V.fromList <$> multiProps (elProps e)))

parseASCIIv3 :: forall a. PLYType a => Element -> Parser (VS.Vector (V3 a))
parseASCIIv3 (Element _ n ps@[_,_,_])
  | all samePropType ps = replicateM' n (skip *> (V3 <$> p <*> p <*> p))
  | otherwise = empty
  where t = plyType (undefined::a)
        p = unsafeUnwrap <$> (parseScalar t <* skipSpace)
        samePropType (ScalarProperty t' _) = t == t'
        samePropType (ListProperty _ _) = False
parseASCIIv3 _ = empty

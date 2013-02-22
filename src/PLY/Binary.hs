module PLY.Binary where
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Maybe (isJust, catMaybes)
import Data.Serialize.Get
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Unsafe.Coerce
import PLY.Types

getScalarT :: ScalarT -> Get Scalar
getScalarT Tchar = Schar . fromIntegral <$> getWord8
getScalarT Tuchar = Suchar <$> getWord8
getScalarT Tshort = Sshort . fromIntegral <$> getWord16host
getScalarT Tushort = Sushort <$> getWord16host
getScalarT Tint = Sint . fromIntegral <$> getWord32host
getScalarT Tuint = Suint <$> getWord32host
getScalarT Tfloat = Sfloat . unsafeCoerce <$> getWord32host
getScalarT Tdouble = Sdouble . unsafeCoerce <$> getWord64host

mkScalarParser :: ScalarT -> Get Scalar
mkScalarParser t = getScalarT t

mkListParser :: ScalarT -> Get (Vector Scalar)
mkListParser t =
  getWord8 >>= flip V.replicateM (getScalarT t) . fromIntegral
                                               
mkElParser :: [Property] -> Get (Vector Scalar)
mkElParser props
  | all isJust scalars = let props' = V.fromList (catMaybes scalars)
                             aux i = let parsers = V.map mkScalarParser props'
                                     in parsers ! i
                         in V.generateM (V.length props') aux
  | null (tail props) = let (ListProperty t _) = head props in mkListParser t
  | otherwise = error $ "Binary elements with multiple property "++
                        "lists are not supported!"
  where getScalar (ScalarProperty t _) = Just t
        getScalar _ = Nothing
        scalars = map getScalar props

parseBinElement :: Element -> ByteString -> (Vector (Vector Scalar), ByteString)
parseBinElement (Element _ 0 _) bs = (V.empty, bs)
parseBinElement (Element _ n props) bs = 
  either error id $ runGetState elParser bs 0
  where elParser = V.replicateM n (mkElParser props)

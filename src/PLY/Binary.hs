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
{-
-- Property size in bytes.
scalarBinSize :: ScalarT -> Int
scalarBinSize Tchar   = 1
scalarBinSize Tuchar  = 1
scalarBinSize Tshort  = 2
scalarBinSize Tushort = 2
scalarBinSize Tint    = 4
scalarBinSize Tuint   = 4
scalarBinSize Tfloat  = 4
scalarBinSize Tdouble = 8

staticElSize :: Element -> Maybe Int
staticElSize (Element _ _ props) = go 0 props
  where go !acc [] = Just acc
        go !acc (ScalarProperty t _:ps) = go (acc+scalarBinSize t) ps
        go !acc _ = Nothing


listElSize :: Int -> ScalarT -> ByteString -> Int
listElSize n t = go 0 n 
  where sz = scalarBinSize t
        go !acc n' bs = let propsSize = fromIntegral (BS.head bs) * sz
                        in go (propsSize + acc) (n'-1) (BS.drop propsSize bs)

-- For elements that are not lists, we can compute the binary size just from the element header. For elements that are lists, we must look at the data.
elBinSize :: Element -> ByteString -> Int
elBinSize el@(Element _ n props) bs = maybe dynElSize aux $ staticElSize el
  where aux sz = n * sz
        dynElSize = case props of
                      [ListProperty t _] -> listElSize n t bs
                      _ -> error $ "Binary elements with multiple property "++
                                   "lists are not supported!"

parseBinEl :: Element -> ByteString -> (Vector (Vector Scalar), ByteString)
parseBinEl el bs = let n = elBinSize el
                   in (BS.drop n)
-}
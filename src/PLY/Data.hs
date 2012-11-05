{-# LANGUAGE ScopedTypeVariables #-}
-- |The loading of a @ply@ file is broken down into two steps: header
-- parsing, and data loading. The 'loadPLY' function will, if
-- successful, return a data structure that may be queried to extract
-- numeric data using 'loadElements' and 'loadElementsV3'. For example, 
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Control.Monad ((>=>))
-- > import Data.ByteString (ByteString)
-- > import qualified Data.Vector.Storable as VS
-- > import Linear.V3
-- >
-- > loadVerts :: ByteString -> Either String (VS.Vector (V3 Float))
-- > loadVerts = loadPLY >=> loadElementsV3 "vertex"
-- 
-- To load all vertex data from a series of @ply@ files identified by
-- a @.conf@ file, consider using,
--
-- > loadConf :: FilePath -> IO (Either [String] (VS.Vector (V3 Float)))
-- > loadConf confFile = loadMeshesV3 confFile "vertex"
--
module PLY.Data (PLYData, loadPLY, loadElements, loadElementsV3, 
                 loadMeshesV3) where
import Control.Applicative
import Control.Concurrent.ParallelIO (parallel)
import Control.Lens (view)
import Control.Monad ((>=>))
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Either (rights, partitionEithers)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Linear.Matrix (mkTransformation, (!*))
import Linear.V3
import Linear.V4 (vector)
import System.Directory (canonicalizePath)
import System.FilePath (takeDirectory, (</>))

import PLY.Conf
import PLY.Internal.Parsers (line, parseSkip, skip, multiProps, 
                             parseScalar, header)
import PLY.Types

type Header = (Format, [Element])

-- |A PLY header and the associated raw data. Use 'loadElements' or
-- 'loadElementsV3' to extract a particular element array.
newtype PLYData = PLYData (ByteString, Header)

instance Show PLYData where
  show (PLYData (_,h)) = "PLYData <bytes> " ++ show h

parseASCII :: Element -> Parser (Vector (Vector Scalar))
parseASCII e = V.replicateM (elNum e) 
                            (skip *> (V.fromList <$> multiProps (elProps e)))

parseASCIIv3 :: forall a. PLYType a => Element -> Parser (VS.Vector (V3 a))
parseASCIIv3 (Element _ n ps@[_,_,_])
  | all samePropType ps = VS.replicateM n (skip *> (V3 <$> p <*> p <*> p))
  | otherwise = empty
  where t = plyType (undefined::a)
        p = unsafeUnwrap <$> (parseScalar t <* skipSpace)
        samePropType (ScalarProperty t' _) = t == t'
        samePropType (ListProperty _ _) = False
parseASCIIv3 _ = empty

-- |If the PLY header is successfully parsed, the 'PLYData' value
-- returned may be used with 'loadElements' and 'loadElementsV3' to
-- extract data.
loadPLY :: ByteString -> Either String PLYData
loadPLY = aux . parse header
  where aux (Fail _t ctxt msg) = Left $ "Parse failed: "++msg++" in "++show ctxt
        aux (Partial _) = Left "Incomplete header"
        aux (Done t r) = Right $ PLYData (t, r)

-- |@loadElements elementName ply@ loads a 'Vector' of each instance
-- of the requested element array. If you are extracted 3D data,
-- consider using 'loadElementsV3'.
loadElements :: ByteString -> PLYData -> 
                Either String (Vector (Vector Scalar))
loadElements n (PLYData (body, (ASCII, ess))) = go ess body
  where go [] _ = Left "Unknown element"
        go (e:es) b | elName e == n = parseOnly (parseASCII e) b
                    | otherwise = go es $
                                  parseSkip (count (elNum e) line *> pure ()) b
loadElements _ _ = error "Binary PLY is unsupported"

-- |Like 'loadElements', but restricted to 3D vectors. When it can be
-- used, this function is much more efficient than 'loadElements'.
loadElementsV3 :: PLYType a => ByteString -> PLYData -> 
                  Either String (VS.Vector (V3 a))
loadElementsV3 n (PLYData (body, (ASCII, ess))) = go ess body
  where go [] _ = Left "Unknown element"
        go (e:es) b | elName e == n = parseOnly (parseASCIIv3 e) b
                    | otherwise = go es $
                                  parseSkip (count (elNum e) line *> pure ()) b
loadElementsV3 _ _ = error "Binary PLY is unsupported"

-- |Load all meshes identified by a @.conf@ file in parallel, and
-- transform vertex data into the coordinate frame specified by the
-- @.conf@ file. The application @loadMeshesV3 confFile element@ loads
-- @confFile@ to identify every @ply@ mesh to load. The @ply@ files
-- are loaded from the same directory that contained the @.conf@ file,
-- and the data associated with @element@ (e.g. @\"vertex\"@) is
-- loaded, transformed, and concatenated from all the meshes.
loadMeshesV3 :: forall a. (PLYType a, Fractional a) => 
                FilePath -> ByteString -> IO (Either [String] (VS.Vector (V3 a)))
loadMeshesV3 confFile element = do dir <- takeDirectory <$> 
                                          canonicalizePath confFile
                                   c <- parseConf <$> BS.readFile confFile
                                   either (return . Left . (:[]))
                                          (fmap checkConcat . loadAllMeshes dir)
                                          c
    where checkErrors xs = let (ls,rs) = partitionEithers xs
                           in if null ls then Right rs else Left ls
          checkConcat = fmap VS.concat . checkErrors
          loadMesh :: FilePath -> (ByteString, Transformation Double) -> 
                      IO (Either String (VS.Vector (V3 a)))
          loadMesh d (f, (t,r)) = 
            let m = mkTransformation (fmap realToFrac r) (fmap realToFrac t)
            in (loadPLY 
                >=> loadElementsV3 element
                >=> return . VS.map (view _xyz . (m !*) . vector))
               <$> BS.readFile (d </> BC.unpack f)
          loadAllMeshes dir = parallel . map (loadMesh dir) . meshes

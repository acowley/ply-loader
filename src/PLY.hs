{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
-- |The loading of a @ply@ file is broken down into two steps: header
-- parsing, and data loading. The 'loadPLY' function will, if
-- successful, return a data structure that may be queried to extract
-- numeric data using 'loadElements' and 'loadElementsV3'. For example, 
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Data.Vector.Storable (Vector)
-- > import Linear.V3
-- > import PLY
-- >
-- > loadVerts :: FilePath -> IO (Either String (Vector (V3 Float)))
-- > loadVerts = loadElementsV3 "vertex"
-- 
-- To load all vertex data from a series of @ply@ files identified by
-- a @.conf@ file, consider using,
--
-- > fromConf :: FilePath -> IO (Either String (Vector (V3 Float)))
-- > fromConf = loadConfV3 "vertex"
--
module PLY (-- * Easy loading interface
            loadElements, loadElementsV3, loadConfV3,

            -- * Loading components
            Header, PLYData, loadHeader, preloadPly, plyHeader,
            loadPlyElements, loadPlyElementsV3) where
import Control.Applicative
import Control.Concurrent.ParallelIO (parallel)
import Control.Monad ((>=>))
import Control.Monad.Trans.Error
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Either (partitionEithers)
import Data.Vector (Vector)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import Linear
import System.Directory (canonicalizePath)
import System.FilePath (takeDirectory, (</>))

import PLY.Ascii
import PLY.Binary
import PLY.Conf
import PLY.Internal.Parsers (line, parseSkip, header)
import PLY.Types

type Header = (Format, [Element])

-- |A PLY header and the associated raw data. Use 'loadElements' or
-- 'loadElementsV3' to extract a particular element array.
data PLYData = PLYData !ByteString !Header

instance Show PLYData where
  show (PLYData _ h) = "PLYData <bytes> " ++ show h

-- | Extract the 'Header' from a partially loaded PLY file (as from
-- 'preloadPly').
plyHeader :: PLYData -> Header
plyHeader (PLYData _ h) = h

-- Helper to ensure that that an 'Either' is strict in the argument to
-- the data constructor. This is important to keep Vector operations
-- flowing efficiently.
strictE :: Either a b -> Either a b
strictE l@(Left !_x) = l
strictE r@(Right !_x) = r

-- |Load a PLY header from a file.
loadHeader :: FilePath -> IO (Either String PLYData)
loadHeader = fmap preloadPly . BS.readFile

-- |Attempt to parse a PLY file from the given bytes. If the PLY
-- header is successfully parsed, the 'PLYData' value returned may be
-- used with 'loadElements' and 'loadElementsV3' to extract data.
preloadPly :: ByteString -> Either String PLYData
preloadPly = aux . parse header
  where aux (Fail _t ctxt msg) = Left $ "Parse failed: "++msg++" in "++show ctxt
        aux (Partial _) = Left "Incomplete header"
        aux (Done t r) = Right $ PLYData t r

-- |@loadPlyElements elementName ply@ loads a 'Vector' of each vertex of
-- the requested element array. If you are extracting 3D data,
-- consider using 'loadPlyElementsV3'.
loadPlyElements :: ByteString -> PLYData -> 
                   Either String (Vector (Vector Scalar))
loadPlyElements n (PLYData body (ASCII, ess)) = strictE $ go ess body
  where go [] _ = Left "Unknown element"
        go (e:es) b | elName e == n = parseOnly (parseASCII e) b
                    | otherwise = go es $
                                  parseSkip (count (elNum e) line *> pure ()) b
loadPlyElements n (PLYData body (Binary_LE, ess)) = strictE $ go ess body
  where go [] _ = Left "Unknown element"
        go (e:es) b | elName e == n = Right . fst $ parseBinElement e b
                    | otherwise = go es . snd $ parseBinElement e b
loadPlyElements _ _ = error "Binary PLY is unsupported"
{-# INLINABLE loadPlyElements #-}

-- |@loadElements elementName plyFile@ loads a 'Vector' of each
-- vertex of the requested element array from @plyFile@.
loadElements :: ByteString -> FilePath
             -> IO (Either String (Vector (Vector Scalar)))
loadElements name file =
  (preloadPly >=> loadPlyElements name) <$> BS.readFile file
{-# INLINABLE loadElements #-}

-- |Like 'loadPlyElements', but restricted to 3D vectors. When it can be
-- used, this function is much more efficient than 'loadPlyElements'.
loadPlyElementsV3 :: PLYType a => ByteString -> PLYData -> 
                     Either String (VS.Vector (V3 a))
loadPlyElementsV3 n (PLYData body (ASCII, ess)) = strictE $ go ess body
  where go [] _ = Left "Unknown element"
        go (e:es) b | elName e == n = parseOnly (parseASCIIv3 e) b
                    | otherwise = go es $
                                  parseSkip (count (elNum e) line *> pure ()) b
loadPlyElementsV3 _ _ = error "Binary PLY is unsupported"
{-# INLINABLE loadPlyElementsV3 #-}

-- |Like 'loadElements', but restricted to 3D vectors. When it can
-- be used, this function is much more efficient thatn
-- 'loadPlyElements'.
loadElementsV3 :: PLYType a => ByteString -> FilePath
               -> IO (Either String (VS.Vector (V3 a)))
loadElementsV3 name file =
  (preloadPly >=> loadPlyElementsV3 name) <$> BS.readFile file
{-# INLINABLE loadElementsV3 #-}

type ErrorMsg a = Either String a

-- |Load all meshes identified by a @.conf@ file in parallel, and
-- transform vertex data into the coordinate frame specified by the
-- @.conf@ file. The application @loadMeshesV3 confFile element@ loads
-- @confFile@ to identify every @ply@ mesh to load. The @ply@ files
-- are loaded from the same directory that contained the @.conf@ file,
-- and the data associated with @element@ (e.g. @\"vertex\"@) is
-- loaded, transformed, and concatenated from all the meshes.
loadConfV3 :: forall a. (PLYType a, Fractional a, Conjugate a, RealFloat a)
           => ByteString -> FilePath -> IO (Either String (VS.Vector (V3 a)))
loadConfV3 element confFile = 
  do dir <- takeDirectory <$> canonicalizePath confFile
     runErrorT $
       do c <- ErrorT $ parseConf <$> BS.readFile confFile
          ErrorT $ checkConcat <$> loadAll dir c
    where checkErrors :: [ErrorMsg b] -> ErrorMsg [b]
          checkErrors xs = let (ls,rs) = partitionEithers xs
                           in if null ls then Right rs else Left (unlines ls)
          checkConcat :: [ErrorMsg (VS.Vector (V3 a))]
                      -> ErrorMsg (VS.Vector (V3 a))
          checkConcat = (fmap VS.concat $!) . checkErrors
          loadMesh :: FilePath -> M44 a -> (ByteString, Transformation a)
                   -> IO (ErrorMsg (VS.Vector (V3 a)))
          loadMesh d _cam (f, (t,r)) = 
            -- It is convenient to ignore the camera transformation so
            -- that the object is at the origin.
            let m = (^+^ fmap realToFrac t ) . rotate (conjugate (fmap realToFrac r))
            in fmap (VS.map m) <$> loadElementsV3 element (d </> BC.unpack f)
          loadAll :: FilePath -> Conf a -> IO ([ErrorMsg (VS.Vector (V3 a))])
          loadAll dir (Conf (t,r) ms) = 
            let cam = mkTransformation r t
            in parallel $ map (loadMesh dir cam) ms
{-# INLINABLE loadConfV3 #-}

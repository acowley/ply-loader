{-# LANGUAGE OverloadedStrings #-}
-- |Parse Stanford 3D Scanning Repository \"@.conf@\" files that place
-- individual PLY models into a consistent coordinate frame.
module PLY.Conf (parseConf, Transformation, Conf(..)) where
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Linear.V3
import Linear.Quaternion
import PLY.Internal.Parsers (line, word)

-- | A 3D transformation represented as a translation vector and a
-- rotation.
type Transformation a = (V3 a, Quaternion a)

-- |A @.conf@ file includes a base transformation matrix, and a
-- list of meshes identified by their file name and a 'Transformation'
-- to place geometry in a consistent coordinate frame.
data Conf a = Conf { camera :: Transformation a
                   , meshes :: [(ByteString, Transformation a)] }
              deriving Show

-- |Parse a 3D translation vector followed by a quaternion.
transformation :: Fractional a => Parser (V3 a, Quaternion a)
transformation = (,) <$> vec <*> rotation
  where vec = (\[x,y,z] -> V3 x y z) <$> count 3 (skipSpace *> double')
        rotation = flip Quaternion <$> vec <*> (skipSpace *> double')
        double' = realToFrac <$> double
        --rotation = Quaternion <$> (skipSpace *> double) <*> vec
        --rev (V3 x y z) = V3 z y x

-- |Parse a mesh file specification.
mesh :: Fractional a => Parser (ByteString, Transformation a)
mesh = (,) <$> ("bmesh " *> word) <*> transformation

-- |Parser for a Stanford .conf file.
conf :: Fractional a => Parser (Conf a)
conf = Conf <$> ("camera " *> transformation <* skipSpace)
            <*> (skipSpace *> cybmesh *> many1 (skipSpace *> mesh))
  where cybmesh = skipMany ("mesh " *> line) -- Not a PLY file!

-- |Parse a Stanford .conf file.
parseConf :: Fractional a => ByteString -> Either String (Conf a)
parseConf = parseOnly conf

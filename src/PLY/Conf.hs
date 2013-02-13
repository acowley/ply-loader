{-# LANGUAGE OverloadedStrings #-}
-- |Parse Stanford 3D Scanning Repository \"@.conf@\" files that place
-- individual PLY models into a consistent coordinate frame.
module PLY.Conf (parseConf, Transformation, Conf(..)) where
import Control.Applicative
import Data.Attoparsec.Char8
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
data Conf = Conf { camera :: Transformation Double 
                 , meshes :: [(ByteString, Transformation Double)] }
            deriving Show

-- |Parse a 3D translation vector followed by a quaternion.
transformation :: Parser (V3 Double, Quaternion Double)
transformation = (,) <$> vec <*> rotation
  where vec = (\[x,y,z] -> V3 x y z) <$> count 3 (skipSpace *> double)
        rotation = Quaternion <$> (skipSpace *> double) <*> vec

-- |Parse a mesh file specification.
mesh :: Parser (ByteString, Transformation Double)
mesh = (,) <$> ("bmesh " .*> word) <*> transformation

-- |Parser for a Stanford .conf file.
conf :: Parser Conf
conf = Conf <$> ("camera " .*> transformation <* skipSpace)
            <*> (skipSpace *> cybmesh *> many1 (skipSpace *> mesh))
  where cybmesh = skipMany ("mesh " .*> line) -- Not a PLY file!

-- |Parse a Stanford .conf file.
parseConf :: ByteString -> Either String Conf
parseConf = parseOnly conf

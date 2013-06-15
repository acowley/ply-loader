{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module PLY.Internal.Parsers where
import Control.Applicative
import Data.Attoparsec.Char8 hiding (char)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import PLY.Types

-- |Skip white space, comments, and obj_info lines.
skip :: Parser ()
skip = skipSpace *> ((ignore *> line *> skip) <|> pure ())
  where ignore = string "comment " <|> string "obj_info "

-- |Parse a PLY file format line
format :: Parser Format
format = "format" .*> skipSpace *> (ascii <|> le <|> be)
  where ascii = "ascii 1.0" .*> pure ASCII
        le = "binary_little_endian 1.0" .*> pure Binary_LE
        be = "binary_big_endian 1.0" .*> pure Binary_BE

-- * Numeric type parsers

char :: Parser Int8
char = signed decimal

uchar :: Parser Word8
uchar = decimal

int :: Parser Int
int = signed decimal

uint :: Parser Word32
uint = decimal

int16 :: Parser Int16
int16 = signed decimal

uint16 :: Parser Word16
uint16 = decimal

float :: Parser Float
float = realToFrac <$> double

-- | Take everything up to the end of the line
line :: Parser ByteString
line = BC.pack <$> manyTill anyChar endOfLine

scalarProperty :: Parser Property
scalarProperty = ScalarProperty <$> ("property " .*> scalarType) <*> line

scalarType :: Parser ScalarT
scalarType = choice $
             [ "char "   .*> pure Tchar
             , "uchar "  .*> pure Tuchar
             , "short "  .*> pure Tshort
             , "ushort " .*> pure Tushort
             , "int "    .*> pure Tint
             , "uint "   .*> pure Tuint
             , "float "  .*> pure Tfloat
             , "double " .*> pure Tdouble ]

-- |Take the next white space-delimited word.
word :: Parser ByteString
word = skipSpace *> takeTill isSpace <* skipSpace

listProperty :: Parser Property
listProperty = ListProperty <$> ("property list " .*> word *> scalarType)
                            <*> line

-- |Parse a monotyped list of values. All returned 'Scalar' values
-- will be of the type corresponding to the specific 'ScalarT' given.
parseList :: ScalarT -> Parser [Scalar]
parseList t = int >>= flip count (parseScalar t)

property :: Parser Property
property = skip *> (scalarProperty <|> listProperty)

element :: Parser Element
element = Element <$> ("element " .*> takeTill isSpace) 
                  <*> (skipSpace *> int <* skipSpace)
                  <*> many1 property

parseScalar :: ScalarT -> Parser Scalar
parseScalar Tchar   = Schar   <$> char
parseScalar Tuchar  = Suchar  <$> uchar
parseScalar Tshort  = Sshort  <$> int16
parseScalar Tushort = Sushort <$> uint16
parseScalar Tint    = Sint    <$> int
parseScalar Tuint   = Suint   <$> uint
parseScalar Tfloat  = Sfloat  <$> float
parseScalar Tdouble = Sdouble <$> double

-- |Parse a flat property list
multiProps :: [Property] -> Parser [Scalar]
multiProps = go []
  where go acc [] = pure (reverse acc)
        go acc (ScalarProperty t _:ps) = do !x <- parseScalar t
                                            skipSpace
                                            go (x:acc) ps
        go _ (ListProperty t _:_) = int <* skipSpace >>= flip count (parseScalar t)

-- FIXME: Support for list properties assumes that an element will not
-- have any other properties if it has a list property!

-- |Parse a PLY header.
header :: Parser (Format, [Element])
header = (,) <$> preamble <*> elements <*. "end_header" <* endOfLine
  where preamble = "ply" .*> skip *> format
        elements = many1 (skip *> element <* skipSpace)

-- |Advance a 'ByteString' to where a given 'Parser' finishes. An
-- 'error' is raised if the parser fails to complete.
parseSkip :: Parser a -> ByteString -> ByteString
parseSkip = (aux .) . parse
  where aux (Fail _ _ msg) = error $ "parseSkip failed: "++msg
        aux (Partial _) = error $ "Incomplete data"
        aux (Done t _) = t

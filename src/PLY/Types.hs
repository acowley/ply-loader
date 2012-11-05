module PLY.Types (module Data.Int, module Data.Word,
                  Format(..), Scalar(..), ScalarT(..),
                  Property(..), Element(..), PLYType(..), Storable) where
import Data.ByteString (ByteString)
import Data.Int
import Data.Word
import Foreign.Storable (Storable)

data Format = ASCII | Binary_LE | Binary_BE deriving Show

data Scalar = Schar Int8 
            | Suchar Word8
            | Sshort Int16
            | Sushort Word16
            | Sint Int
            | Suint Word32
            | Sfloat Float
            | Sdouble Double
              deriving Show

data ScalarT = Tchar | Tuchar | Tshort | Tushort | Tint | Tuint 
             | Tfloat | Tdouble deriving (Eq,Show)

data Property = ScalarProperty ScalarT ByteString
              | ListProperty ScalarT ByteString
                deriving Show

data Element = Element { elName  :: ByteString
                       , elNum   :: Int
                       , elProps :: [Property] } deriving Show

class Storable a => PLYType a where
  plyType :: a -> ScalarT
  unsafeUnwrap :: Scalar -> a

instance PLYType Float where
  plyType _ = Tfloat
  unsafeUnwrap (Sfloat x) = x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a Float"

instance PLYType Double where
  plyType _ = Tdouble
  unsafeUnwrap (Sdouble x) = x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a Double"

instance PLYType Int where
  plyType _ = Tint
  unsafeUnwrap (Sint x) = x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as an Int"

module PLY.Types (module Data.Int, module Data.Word,
                  Format(..), Scalar(..), ScalarT(..),
                  Property(..), Element(..), PLYType(..), Storable) where
import Data.ByteString (ByteString)
import Data.Int
import Data.Word
import Foreign.C.Types (CFloat, CDouble, CInt, CUChar)
import Foreign.Storable (Storable)

data Format = ASCII | Binary_LE | Binary_BE deriving Show

data Scalar = Schar   {-# UNPACK #-}!Int8 
            | Suchar  {-# UNPACK #-}!Word8
            | Sshort  {-# UNPACK #-}!Int16
            | Sushort {-# UNPACK #-}!Word16
            | Sint    {-# UNPACK #-}!Int
            | Suint   {-# UNPACK #-}!Word32
            | Sfloat  {-# UNPACK #-}!Float
            | Sdouble {-# UNPACK #-}!Double
              deriving Show

data ScalarT = Tchar | Tuchar | Tshort | Tushort | Tint | Tuint 
             | Tfloat | Tdouble deriving (Eq,Show)

data Property = ScalarProperty !ScalarT !ByteString
              | ListProperty !ScalarT !ByteString
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

instance PLYType CFloat where
  plyType _ = Tfloat
  unsafeUnwrap (Sfloat x) = realToFrac x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a CFloat"

instance PLYType Double where
  plyType _ = Tdouble
  unsafeUnwrap (Sdouble x) = x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a Double"

instance PLYType CDouble where
  plyType _ = Tdouble
  unsafeUnwrap (Sdouble x) = realToFrac x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a CDouble"

instance PLYType Int where
  plyType _ = Tint
  unsafeUnwrap (Sint x) = x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as an Int"

instance PLYType CInt where
  plyType _ = Tint
  unsafeUnwrap (Sint x) = fromIntegral x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as an CInt"

instance PLYType Word8 where
  plyType _ = Tuchar
  unsafeUnwrap (Suchar x) = x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a Word8"

instance PLYType CUChar where
  plyType _ = Tuchar
  unsafeUnwrap (Suchar x) = fromIntegral x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a CUChar"

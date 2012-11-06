{-# LANGUAGE OverloadedStrings #-}
-- |Executable that writes all 3D vertices found in all @PLY@ files
-- indicated by a @.conf@ file to a single binary file.
module Main (main) where
import Control.Monad (when)
import qualified Data.Vector.Storable as VS
import Linear.V3
import PLY.Data
import System.Environment (getArgs)
import System.IO (withBinaryFile, IOMode(WriteMode), hPutBuf)

main :: IO ()
main = do args@(~[confFile, outputFile]) <- getArgs
          when (length args /= 2)
               (error "Usage: ply2bin confFile outputFile")
          Right pts <- loadMeshesV3 confFile "vertex" 
                         :: IO (Either [String] (VS.Vector (V3 Float)))
          putStrLn $ "Loaded "++show (VS.length pts)++" vertices"
          withBinaryFile outputFile WriteMode $ \h ->
            VS.unsafeWith pts $ \ptr ->
              hPutBuf h ptr (VS.length pts * 12)

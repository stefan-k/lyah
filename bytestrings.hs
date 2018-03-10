module Main where

{- import qualified Data.ByteString as S -}
import qualified Data.ByteString.Lazy as B
import System.Environment

main :: IO ()
main = do
  (fileName1:fileName2:_) <- getArgs
  copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
  contents <- B.readFile source
  B.writeFile dest contents

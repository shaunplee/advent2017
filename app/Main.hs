module Main where

import           Checksum

main :: IO ()
main = do
  content <- getContents
  print $ checksumDiv content

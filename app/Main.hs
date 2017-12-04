module Main where

import           Checksum
import           Passphrases

main :: IO ()
main = do
  content <- getContents
  print $ validPassphrasesAna content

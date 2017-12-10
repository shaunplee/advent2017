module Main where

import           Checksum
import           Circus
import           Hash
import           Maze
import           Passphrases
import           Registers
import           Stream

main :: IO ()
main = do
  content <- getContents
  print $ hash content

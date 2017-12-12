module Main where

import           Checksum
import           Circus
import           Hash
import           Hex
import           Maze
import           Passphrases
import           Pipes
import           Registers
import           Stream

main :: IO ()
main = do
  content <- getContents
  print $ countGroups content

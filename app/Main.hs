module Main where

import           Checksum
import           Circus
import           Maze
import           Passphrases
import           Registers

main :: IO ()
main = do
  content <- getContents
  print $ runProgramMax content

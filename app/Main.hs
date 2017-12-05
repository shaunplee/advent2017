module Main where

import           Checksum
import           Maze
import           Passphrases

main :: IO ()
main = do
  content <- getContents
  print $ jumpsM content

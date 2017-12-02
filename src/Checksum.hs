module Checksum where

import           Text.Trifecta

sampleInput = "5 1 9 5\n7 5 3\n2 4 6 8"

checksumLine :: String -> Int
checksumLine l = let ds = map read (words l) :: [Int]
                 in maximum ds - minimum ds

checksum :: String -> Int
checksum ss = sum $ map checksumLine (lines ss)

sampleDiv = "5 9 2 8\n9 4 7 3\n3 8 6 5"

checksumDivLine :: String -> Int
checksumDivLine l =
  let ds = map read (words l) :: [Int]
  in fst $ head $ filter divides [(uncurry divMod) (x, y) | x <- ds, y <- ds]
  where divides (a, b) = b == 0 && a /= 1

checksumDiv :: String -> Int
checksumDiv ss = sum $ map checksumDivLine (lines ss)

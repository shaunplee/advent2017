module Maze where

import qualified Data.Vector.Unboxed as V


jumps :: [Int] -> Int
jumps is = go (V.fromList is) 0 0
  where
    go :: V.Vector Int -> Int -> Int -> Int
    go is p s =
      if p < 0 || p >= V.length is
        then s
        else let jmp = (is V.! p)
             in go (is V.// [(p, jmp + 1)]) (p + jmp) (s + 1)

jumpsL :: String -> Int
jumpsL s = jumps (map read (words s) :: [Int])

jumps2 :: [Int] -> Int
jumps2 is = go (V.fromList is) 0 0
  where
    go :: V.Vector Int -> Int -> Int -> Int
    go is p s =
      if p < 0 || p >= V.length is
        then s
        else let jmp = (is V.! p)
                 newOffset = if jmp >= 3 then jmp - 1 else jmp + 1
             in go (is V.// [(p, newOffset)]) (p + jmp) (s + 1)

jumpsM :: String -> Int
jumpsM s = jumps2 (map read (words s) :: [Int])

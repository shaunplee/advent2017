module Defragment where

import           Data.Digits
import           Data.List   (foldl')
import qualified Data.Set    as S
import           Hash

import           Debug.Trace

testInput = "flqrgnkx"

myInput = "jzgqcdpd"

hexToInt :: Char -> Int
hexToInt 'a' = 10
hexToInt 'b' = 11
hexToInt 'c' = 12
hexToInt 'd' = 13
hexToInt 'e' = 14
hexToInt 'f' = 15
hexToInt x   = read [x]

hexToBin :: Char -> [Int]
hexToBin x = let b = digits 2 (hexToInt x)
             in replicate (4 - length b) 0 ++ b

unKnot :: String -> [String]
unKnot s = map (\x -> knotHash (s ++ "-" ++ show x)) [0..127]

unKnotB :: [String] -> [[Int]]
unKnotB = map (concatMap hexToBin)

countOccupied :: [[Int]] -> Int
countOccupied x = sum $ map sum x

occupied :: [[Int]] -> (Int, Int) -> Bool
occupied occ (x, y) = (occ !! y) !! x == 1

countRegions :: [[Int]] -> Int
countRegions occ = go S.empty (0, 0) 0
  where
    go visited pos groups =
      let (newVisited, newGroups) =
            if pos `S.notMember` visited
              then if occupied occ pos
                   then
                      trace (show pos ++ " " ++ show groups) $
                      (explore occ visited pos, groups + 1)
                   else
                     (S.insert pos visited, groups)
              else (visited, groups)
      in case nextPos pos of
           Nothing -> newGroups
           Just np -> go newVisited np newGroups



explore :: [[Int]] -> S.Set (Int, Int) -> (Int, Int) -> S.Set (Int, Int)
explore occ visited pos =
  let unvisitedOccupied x = (x `S.notMember` visited) && occupied occ x
      ns = filter unvisitedOccupied (neighbors pos)
  in trace (show ns) $ foldl' (explore occ) (S.insert pos visited) ns



nextPos :: (Int, Int) -> Maybe (Int, Int)
nextPos (127, 127) = Nothing
nextPos (127, y)   = Just (0, y + 1)
nextPos (x, y)     = Just (x+1, y)

neighbors :: (Int, Int) -> [(Int, Int)]
--corners
neighbors (0, 0)     = [(0, 1), (1, 0)]
neighbors (0, 127)   = [(0, 126), (1, 127)]
neighbors (127, 0)   = [(126, 0), (127, 1)]
neighbors (127, 127) = [(126, 127), (127, 126)]
--left edge
neighbors (0, y)     = [(0, y - 1), (0, y + 1), (1, y)]
-- right edge
neighbors (127, y)   = [(127, y - 1), (127, y - 1), (126, y)]
--top edge
neighbors (x, 0)     = [(x - 1, 0), (x + 1, 0), (x, 1)]
--bottom edge
neighbors (x, 127)   = [(x - 1, 127), (x + 1, 127), (x, 126)]
--else
neighbors (x, y)     = [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]

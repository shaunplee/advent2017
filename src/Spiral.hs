module Spiral where

import qualified Data.Map as M

-- 2 ( 1,0)
-- 4 (2, 1)
-- 538 (269, 268

data Moving = Up | Down | Lt | Rt

type Pos = (Int, Int)

run :: Integer -> Integer
run x = go (1, 0) (M.insert (0, 0) 1 M.empty) Up
  where
    go :: Pos -> M.Map Pos Integer -> Moving -> Integer
    go p m d =
      let nextVal = sumNeighbors p m
      in if nextVal > x
           then nextVal
           else let (np, mov) = nextPos p d
                in go np (M.insert p nextVal m) mov


sumNeighbors :: Pos -> M.Map Pos Integer -> Integer
sumNeighbors (x, y) m =
  let neighbors =
        [ (x - 1, y + 1)
        , (x, y + 1)
        , (x + 1, y + 1)
        , (x - 1, y)
        , (x + 1, y)
        , (x - 1, y - 1)
        , (x, y - 1)
        , (x + 1, y - 1)
        ]
  in sum $ map (\k -> M.findWithDefault 0 k m) neighbors

nextPos :: Pos -> Moving -> (Pos, Moving)
nextPos (x, y) Up
  | y == x = ((x - 1, y), Lt)
  | otherwise = ((x, y + 1), Up)
nextPos (x, y) Down
  | y == x = ((x + 1, y), Rt)
  | otherwise = ((x, y - 1), Down)
nextPos (x, y) Lt
  | y == -x = ((x, y - 1), Down)
  | otherwise = ((x - 1, y), Lt)
nextPos (x, y) Rt
  | y == -x = ((x + 1, y), Up)
  | otherwise = ((x + 1, y), Rt)

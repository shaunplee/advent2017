module Bank where

import           Data.List           (elemIndex)
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector.Unboxed as V

import           Debug.Trace

redistribute :: V.Vector Int -> V.Vector Int
redistribute cur =
  let p = V.maxIndex cur
      len = V.length cur
      (a, b) = (cur V.! p) `divMod` len
      new = V.map (+ a) (cur V.// [(p, 0)])
      upd v 0 _ = v
      upd v r pos =
        upd (v V.// [(pos, (v V.! pos) + 1)]) (r - 1) ((pos + 1) `mod` len)
  in upd new b ((p + 1) `mod` len)


countRed :: [Int] -> Int
countRed initial = go (V.fromList initial) [] 0
  where
    go :: V.Vector Int -> [V.Vector Int] -> Int -> Int
    go cur hist s =
      if cur `elem` hist
        then s
        else go (redistribute cur) (cur : hist) (s + 1)

countLoop :: [Int] -> Int
countLoop initial = go (V.fromList initial) [] 0
  where
    go :: V.Vector Int -> [V.Vector Int] -> Int -> Int
    go cur hist s =
      if cur `elem` hist
        then countRed (V.toList cur)
        else go (redistribute cur) (cur : hist) (s + 1)

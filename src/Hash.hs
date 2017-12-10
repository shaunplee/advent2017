module Hash where

import           Data.Bits           (xor)
import           Data.Char           (chr, ord)
import           Data.List           (foldl')
import qualified Data.Vector.Unboxed as V
import           Debug.Trace

oneRound :: (V.Vector Int, Int, Int) -> [Int] -> (V.Vector Int, Int, Int)
oneRound = go
  where
    go :: (V.Vector Int, Int, Int) -> [Int] -> (V.Vector Int, Int, Int)
    go (h, p, skip) [] = (h, p, skip)
    go (h, p, skip) (l:ls) =
      go (step h p l, (p + l + skip) `mod` 256, skip + 1) ls
    step :: V.Vector Int -> Int -> Int -> V.Vector Int
    step v pos len =
      let sl = V.slice pos len (v V.++ v)
          r = V.reverse sl
      in V.update v (V.zip (V.fromList (map (`mod` 256) [pos .. pos + len])) r)

hash :: [Int] -> V.Vector Int
hash ls = go ((V.fromList [0..255]), 0, 0) [1..64]
  where go (h, _, _) []     = h
        go (h, p, s) (_:rs) = go (oneRound (h, p, s) ls) rs

input :: [Int]
input = [97, 167, 54, 178, 2, 11, 209, 174, 119, 248, 254, 0, 255, 1, 64, 190]

end :: [Int]
end = [17, 31, 73, 47, 23]

knotHash :: String -> String
knotHash s = let ins = map ord s ++ end
                 sh = hash ins
                 dh = map (\p -> V.foldl1 xor (V.slice p 16 sh)) [0,16..248]
             in concatMap intToHex dh

intToHex :: Int -> String
intToHex x = let (a, b) = x `divMod` 16
                 u = hMap V.! a
                 l = hMap V.! b
             in [u,l]

hMap :: V.Vector Char
hMap = V.fromList (['0' .. '9'] ++ ['a' .. 'f'])

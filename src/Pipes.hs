{-# LANGUAGE Strict #-}
module Pipes where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Text.Trifecta

parseLine :: String -> (Integer, [Integer])
parseLine s = case parseString conn mempty s of
  Success x -> x
  Failure e -> error $ show e

conn :: Parser (Integer, [Integer])
conn = do
  x <- decimal
  _ <- string " <-> "
  cs <- commaSep decimal
  return (x, cs)


countZeroGroup :: String -> Int
countZeroGroup s = let cs = M.fromAscList $ map parseLine (lines s)
                   in length $ buildGroup 0 cs

buildGroup :: Integer -> M.Map Integer [Integer] -> S.Set Integer
buildGroup s cs = go (S.singleton s) (cs M.! s)
  where go :: S.Set Integer -> [Integer] -> S.Set Integer
        go visited [] = visited
        go visited (n:ns) = if n `S.member` visited
          then go visited ns
          else go (n `S.insert` visited) (ns ++ cs M.! n)

testInput = "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5"

countGroups :: String -> Int
countGroups s =
  let cs = M.fromAscList $ map parseLine (lines s)
  in go cs 0
  where
    go :: M.Map Integer [Integer] -> Int -> Int
    go cs count =
      if M.null cs
        then count
        else let cur = buildGroup (head $ M.keys cs) cs
             in go (M.filterWithKey (\k _ -> k `notElem` cur) cs) (count + 1)

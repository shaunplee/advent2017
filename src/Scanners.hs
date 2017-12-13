module Scanners where

import           Data.List     (foldl', intercalate)
import           Text.Trifecta

type Layer = (Integer, Integer)

layer :: Layer -> Integer
layer = fst

range :: Layer -> Integer
range = snd

testInput = "0: 3\n1: 2\n4: 4\n6: 4"

testList = map parseLine (lines testInput)

layerParser :: Parser Layer
layerParser = do
  l <- decimal
  _ <- string ": "
  r <- decimal
  return (l, r)

parseLine :: String -> Layer
parseLine s = case parseString layerParser mempty s of
  Success x -> x
  Failure e -> error $ show e

computeSeverity :: String -> Integer
computeSeverity c = let inp = map parseLine (lines c)
                    in sum (map severity inp)
  where severity :: Layer -> Integer
        severity l = if layer l `mod` ((2 * range l) - 2) == 0
                     then layer l * range l
                     else 0

minDelay :: String -> Integer
minDelay c =
  let inl = map parseLine (lines c)
      go d =
        if any (\l -> (layer l + d) `mod` ((2 * range l) - 2) == 0) inl
          then go (d + 1)
          else d
  in go 0

module Hex where

import           Control.Applicative ((<|>))
import           Data.List           (foldl', scanl')
import           Text.Trifecta

data Dir = N | Ne | Se | S | Sw | Nw
  deriving Show

type Coord = (Int, Int, Int)

move :: Coord -> Dir -> Coord
move (x, y, z) N  = (x,     y + 1, z - 1)
move (x, y, z) Ne = (x + 1, y,     z - 1)
move (x, y, z) Nw = (x - 1, y + 1, z    )
move (x, y, z) S  = (x,     y - 1, z + 1)
move (x, y, z) Se = (x + 1, y - 1, z    )
move (x, y, z) Sw = (x - 1, y,     z + 1)

distanceToOrigin :: Coord -> Int
distanceToOrigin (0, 0, 0) = 0
distanceToOrigin (x, y, z)
  | (x >= z) && z >= y = 1 + distanceToOrigin (x - 1, y + 1, z    )
  | (y >= z) && z >= x = 1 + distanceToOrigin (x + 1, y - 1, z    )
  | (x >= y) && y >= z = 1 + distanceToOrigin (x - 1, y,     z + 1)
  | (z >= y) && y >= x = 1 + distanceToOrigin (x + 1, y,     z - 1)
  | (y >= x) && x >= z = 1 + distanceToOrigin (x,     y - 1, z + 1)
  | (z >= x) && x >= y = 1 + distanceToOrigin (x,     y + 1, z - 1)


parsePath :: String -> [Dir]
parsePath s = case parseString pathParser mempty s of
  Success x -> x
  Failure e -> error $ show e

pathParser :: Parser [Dir]
pathParser = commaSep dirParser

dirParser :: Parser Dir
dirParser = do
  d <- some $ oneOf "news"
  return $ case d of
    "n"  -> N
    "s"  -> S
    "ne" -> Ne
    "nw" -> Nw
    "se" -> Se
    "sw" -> Sw

countSteps :: String -> Int
countSteps s = distanceToOrigin $ foldl' move (0,0,0) (parsePath s)

maxDistance :: String -> Int
maxDistance s = maximum (map snd res)
  where
    res =
      scanl'
        (\(pos, m) dir ->
           let newPos = move pos dir
           in (newPos, distanceToOrigin newPos))
        ((0, 0, 0), 0)
        (parsePath s)

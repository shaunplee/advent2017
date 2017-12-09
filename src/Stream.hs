module Stream where

import           Control.Applicative ((<|>))
import           Debug.Trace
import           Text.Trifecta

removeBang :: String -> String
removeBang [] = []
removeBang s = let (front, back) = Prelude.span (/= '!') s
  in front ++ removeBang (drop 2 back)

data Stream
  = Group [Stream]
  | Garbage String
  deriving (Show)


parseStream :: Parser Stream
parseStream = try parseGroup <|> parseGarbage

parseGroup :: Parser Stream
parseGroup = do
  gs <- braces (commaSep parseStream)
  return $ Group gs

parseGarbage :: Parser Stream
parseGarbage = do
  g <- angles (many $ noneOf ">")
  return $ Garbage g

scoreStream :: Int -> Stream -> Int
scoreStream _ (Garbage _) = 0
scoreStream p (Group s)   = p + sum (map (scoreStream (p + 1)) s)

parseInput :: String -> Stream
parseInput s = case parseString parseStream mempty s of
  Success x -> x
  Failure e -> error (show e)

score :: String -> Int
score = scoreStream 1 . parseInput . removeBang

countG :: Stream -> Int
countG (Garbage g) = length g
countG (Group s)   = sum (map countG s)

countGarbage :: String -> Int
countGarbage = countG . parseInput . removeBang

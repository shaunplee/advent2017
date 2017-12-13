module Scanners where

import           Data.List       (foldl', intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Vector     as V
import           Text.Trifecta

import           Debug.Trace

data Layer = Layer
  { layer :: Integer
  , range :: Integer
  , pos   :: Integer
  , dir   :: Dir
  } deriving (Eq)

instance Ord Layer where
  (Layer d1 _ _ _) <= (Layer d2 _ _ _) = d1 <= d2

instance Show Layer where
  show (Layer l r p d) = "d:" ++ show l ++ " r:" ++ show r ++ " p:" ++ show p

data Dir = Up | Dn
  deriving (Eq, Show)

layerParser :: Parser Layer
layerParser = do
  l <- decimal
  _ <- string ": "
  r <- decimal
  return $ Layer l r 0 Dn

parseLine :: String -> Layer
parseLine s = case parseString layerParser mempty s of
  Success x -> x
  Failure e -> error $ show e

computeSeverity :: String -> Integer
computeSeverity c =
  let s = M.fromList (map ((\x -> (layer x, x)) . parseLine) (lines c))
  in goSeverity (s, 0) [0 .. (layer $ maximum s)]

goSeverity :: (M.Map Integer Layer, Integer) -> [Integer] -> Integer
goSeverity (ls, sev) []     = sev
goSeverity (ls, sev) (p:ps) = goSeverity (oneStep p (ls, sev)) ps

oneStep :: Integer ->
           (M.Map Integer Layer, Integer) ->
           (M.Map Integer Layer, Integer)
oneStep p (ls, sev) = --trace (show p ++ ": " ++ show sev ++ show (M.lookup p ls)) $
  let ml = M.lookup p ls
      lSev =
        case ml of
          Nothing -> 0
          Just l ->
            if pos l == 0
              then let sv = layer l * range l in if sv == 0 then 1 else sv
              else 0
      newLs = M.map updatePos ls
  in --trace (intercalate "\n" (map show (M.elems newLs)))
    (newLs, sev + lSev)

updatePos :: Layer -> Layer
updatePos (Layer l r 0 _) = Layer l r 1 Dn
updatePos (Layer l r p d) =
  if r - 1 == p
    then Layer l r (p - 1) Up
    else case d of
           Up -> Layer l r (p - 1) Up
           Dn -> Layer l r (p + 1) Dn

testInput = "0: 3\n1: 2\n4: 4\n6: 4"

minDelay :: String -> Int
minDelay c =
  let s = M.fromList (map ((\x -> (layer x, x)) . parseLine) (lines c))
  in go s 0
  where
    go s d =
      if goSeverity (s, 0) [0 .. (layer $ maximum s)] == 0
        then d
        else go (M.map updatePos s) (d + 1)

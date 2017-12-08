module Circus where

import           Control.Applicative
import           Data.List           (groupBy, sortOn)
import           Data.List.Split     (splitOn)
import           Debug.Trace
import           Text.Trifecta

data Gram = Gram {name :: String, weight :: Integer}
  deriving Show

data Tree = Leaf Gram
  | Branch Gram [Tree]
  | Empty
  deriving Show

data Node = LNode String Integer
  | BNode String Integer [String]
  deriving Show

getName :: Node -> String
getName (LNode name _)   = name
getName (BNode name _ _) = name

getTreeName :: Tree -> String
getTreeName (Branch (Gram n _) _) = n
getTreeName (Leaf (Gram n _))     = n

lookupNode :: [Node] -> String -> Node
lookupNode ns node = head $ filter (\x -> getName x == node) ns

findParent :: Tree -> String -> Tree
findParent t n = go [t]
  where
    go :: [Tree] -> Tree
    go (Leaf _:ts) = go ts
    go (x@(Branch _ lvs):ts) =
      if n `elem` (map getTreeName lvs)
        then x
        else go (ts ++ lvs)

parseLine :: String -> Node
parseLine input = case parseString (try parseBNode <|> parseLNode) mempty input
                  of Success x -> x
                     Failure e -> error $ show e

parseLNode :: Parser Node
parseLNode = do
  var <- some letter
  _ <- string " ("
  wgt <- decimal
  _ <- string ")"
  return $ LNode var wgt

parseBNode :: Parser Node
parseBNode = do
  LNode var wgt <- parseLNode
  _ <- string " -> "
  leaves <- some (letter <|> space <|> char ',')
  return $ BNode var wgt (splitOn ", " leaves)


buildTree :: [Node] -> String -> Tree
buildTree nodes n =
  case lookupNode nodes n of
    (LNode nn wgt) -> Leaf $ Gram nn wgt
    (BNode nn wgt leaves) ->
      Branch (Gram nn wgt) (map (buildTree nodes) leaves)

findRoot :: [Node] -> Node
findRoot ns = let bns = filter (\x -> case x of
                                   BNode{} -> True
                                   _       -> False) ns
                  branches = concatMap (\(BNode _ _ br) -> br) bns
              in head $ filter (\(BNode n _ _) -> n `notElem` branches) bns

getRoot :: Tree -> Gram
getRoot (Leaf x)     = x
getRoot (Branch x _) = x

findBottom :: String -> String
findBottom c =
  let BNode root _ _ = findRoot $ map parseLine (lines c)
  in root

findWeight :: String -> Integer
findWeight c =
  let ns = map parseLine (lines c)
      BNode root _ _ = findRoot ns
      tree = buildTree ns root
      unbalanced = findUnbalanced tree
      (Branch _ leaves) = findParent tree unbalanced
      ((odd:_):(same:_):_) =
        sortOn length $
        groupBy (\x y -> fst x == fst y) $ map (\x -> (weigh x, x)) leaves
  in (fst same - fst odd) +
     weight
       (case snd odd of
          Leaf g     -> g
          Branch g _ -> g)



weigh :: Tree -> Integer
weigh (Leaf (Gram _ w))       = w
weigh (Branch (Gram _ w) lvs) = w + sum (map weigh lvs)

findUnbalanced :: Tree -> String
findUnbalanced (Branch (Gram n wt) lvs) =
  let ws =
        sortOn length $
        groupBy (\x y -> fst x == fst y) (zip (map weigh lvs) lvs)
  in if length ws > 1
       then findUnbalanced $ (snd . head . head) ws
       else n


testInput = "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"

testTree = let nodes = map parseLine (lines testInput)
               BNode root _ _ = findRoot nodes
  in buildTree nodes root

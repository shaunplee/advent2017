module Registers where

import           Control.Applicative ((<|>))
import           Data.List           (foldl')
import qualified Data.Map            as M
import           Text.Trifecta

type Reg = String

data Op = Inc | Dec
  deriving Show

data Cond = Neq
          | Eq
          | Geq
          | Leq
          | Gt
          | Lt
  deriving Show

data Instr = Instr Reg Op Integer Cond Reg Integer
  deriving Show


parseLine :: String -> Instr
parseLine input = case parseString parseInstr mempty input
                  of Success x -> x
                     Failure e -> error $ show e

parseInstr :: Parser Instr
parseInstr = do
  reg <- parseRegister <* space
  op <- parseOp <* space
  arg <- integer
  _ <- string "if "
  test <- parseRegister
  cond <- parseCond
  thresh <- integer
  return $ Instr reg op arg cond test thresh

parseRegister :: Parser Reg
parseRegister = some letter

parseOp :: Parser Op
parseOp = do
  op <- try (string "inc") <|> string "dec"
  return $
    if op == "inc"
      then Inc
      else Dec

parseCond :: Parser Cond
parseCond = do
  op <- try (string " != ") <|>
        try (string " == ") <|>
        try (string " >= ") <|>
        try (string " <= ") <|>
        try (string " > ") <|>
        try (string " < ")
  return $ case op of
          " != " -> Neq
          " == " -> Eq
          " >= " -> Geq
          " <= " -> Leq
          " > "  -> Gt
          " < "  -> Lt

runProgram :: String -> Integer
runProgram c =
  let is = map parseLine (lines c)
      regs = go M.empty is
      go :: M.Map String Integer -> [Instr] -> M.Map String Integer
      go m []      = m
      go m (i:ins) = go (executeInst m i) ins
  in maximum $ map snd (M.toList regs)

runProgramMax :: String -> Integer
runProgramMax c =
  let is = map parseLine (lines c)
      regs = go M.empty is 0
      go :: M.Map String Integer -> [Instr] -> Integer -> Integer
      go m [] max = max
      go m (i:ins) max =
        let newRegs = executeInst m i
            nMax =
              if length newRegs > 0
                then maximum $ map snd (M.toList newRegs)
                else 0
        in go
             newRegs
             ins
             (if nMax > max
                then nMax
                else max)
  in regs


executeInst :: M.Map String Integer -> Instr -> M.Map String Integer
executeInst m (Instr reg op amt cond tst thresh) =
  if evalCond m cond tst thresh
    then case op of
           Inc -> M.insert reg ((M.findWithDefault 0 reg m) + amt) m
           Dec -> M.insert reg ((M.findWithDefault 0 reg m) - amt) m
    else m

evalCond :: M.Map String Integer -> Cond -> Reg -> Integer -> Bool
evalCond m Neq r thr = (M.findWithDefault 0 r m) /= thr
evalCond m Eq  r thr = (M.findWithDefault 0 r m) == thr
evalCond m Geq r thr = (M.findWithDefault 0 r m) >= thr
evalCond m Leq r thr = (M.findWithDefault 0 r m) <= thr
evalCond m Gt  r thr = (M.findWithDefault 0 r m) > thr
evalCond m Lt  r thr = (M.findWithDefault 0 r m) < thr

testInput = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"

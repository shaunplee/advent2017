module Passphrases where

import           Data.List (sort)

validPassphrase :: String -> Bool
validPassphrase pp =
  let ws = words pp
  in go ws
  where
    go :: [String] -> Bool
    go [_]      = True
    go (pw:pws) = notElem pw pws && go pws

validPassphrases :: String -> Int
validPassphrases = length . filter validPassphrase . lines

validPassphraseAna :: String -> Bool
validPassphraseAna pp =
  let ws = map sort (words pp)
  in go ws
  where
    go :: [String] -> Bool
    go [_]      = True
    go (pw:pws) = notElem pw pws && go pws

validPassphrasesAna :: String -> Int
validPassphrasesAna = length . filter validPassphraseAna . lines

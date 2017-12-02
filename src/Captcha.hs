module Captcha where

import           Data.Char (digitToInt)

revCaptcha :: String -> Int
revCaptcha x = let is = map digitToInt x
                   js = drop 1 is ++ take 1 is
               in sum $ map (\(y,z) -> if y == z then y else 0) (zip is js)

revCaptchaTwo :: String -> Int
revCaptchaTwo x = let is = map digitToInt x
                      step = length is `div` 2
                      js = drop step is ++ take step is
                  in sum $ map (\(y,z) -> if y == z then y else 0) (zip is js)

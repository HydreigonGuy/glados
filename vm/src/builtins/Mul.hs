module Mul(hydMul) where

import GetInt

hydMul :: [ String ] -> String
hydMul [a, b] = case (getInt a, getInt b) of
    (Just (x), Just (y)) -> (toEnum 126 :: Char):toStr (x * y)
    _ -> [(toEnum 0 :: Char)]
hydMul _ = [(toEnum 0 :: Char)]

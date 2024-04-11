module Mod(hydMod) where

import GetInt

hydMod :: [ String ] -> String
hydMod [a, b] = case (getInt a, getInt b) of
    (Just (x), Just (y)) -> (toEnum 126 :: Char):toStr (x `mod` y)
    _ -> [(toEnum 0 :: Char)]
hydMod _ = [(toEnum 0 :: Char)]

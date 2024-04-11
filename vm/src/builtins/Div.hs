module Div(hydDiv) where

import GetInt

hydDiv :: [ String ] -> String
hydDiv [a, b] = case (getInt a, getInt b) of
    (Just (x), Just (y)) -> (toEnum 126 :: Char):toStr (x `div` y)
    _ -> [(toEnum 0 :: Char)]
hydDiv _ = [(toEnum 0 :: Char)]

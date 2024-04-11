module Add(hydAdd) where

import GetInt

hydAdd :: [ String ] -> String
hydAdd [a, b] = case (getInt a, getInt b) of
    (Just (x), Just (y)) -> (toEnum 126 :: Char):toStr (x + y)
    _ -> [(toEnum 0 :: Char)]
hydAdd _ = [(toEnum 0 :: Char)]

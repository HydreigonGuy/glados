module Sub(hydSub) where

import GetInt

hydSub :: [ String ] -> String
hydSub [a, b] = case (getInt a, getInt b) of
    (Just (x), Just (y)) -> (toEnum 126 :: Char):toStr (x - y)
    _ -> [(toEnum 0 :: Char)]
hydSub _ = [(toEnum 0 :: Char)]

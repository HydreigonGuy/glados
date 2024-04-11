module Equal(hydEqual) where

import GetInt

hydEqual :: [ String ] -> String
hydEqual [[], []] = [(toEnum 1 :: Char),(toEnum 1 :: Char)]
hydEqual [(x:xs), (y:ys)]
    | x == y = hydEqual [xs, ys]
    | otherwise = [(toEnum 1 :: Char),(toEnum 0 :: Char)]
hydEqual _ = [(toEnum 1 :: Char),(toEnum 0 :: Char)]

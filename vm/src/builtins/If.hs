module If (hydIf) where

import Data.Char

checkIfTrue :: String -> Int
checkIfTrue [] = 0
checkIfTrue (x:xs)
    | ord x /= 0 = 1
    | otherwise = checkIfTrue xs

hydIf :: [ String ] -> String
hydIf [a, b, c]
    | checkIfTrue a == 0 = c
    | otherwise = b
hydIf _ = [(toEnum 1 :: Char),(toEnum 0 :: Char)]

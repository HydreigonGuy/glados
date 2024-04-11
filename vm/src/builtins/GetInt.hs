module GetInt (getInt, toStr) where

import Data.Char

getInt :: String -> Maybe Int
getInt [_, a, b, c, d] = Just (((ord a) * 268435456) + ((ord b) * 16384) + ((ord c) * 128) + (ord d))
getInt _ = Nothing

toStr :: Int -> String
toStr a = [(toEnum (a `div` 268435456) :: Char), (toEnum ((a `mod` 268435456) `div` 16384) :: Char), (toEnum ((a `mod` 16384) `div` 128) :: Char), (toEnum (a `mod` 128) :: Char)]

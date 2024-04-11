module Memory
    ( moveToAddresse, getAddresse
    ) where

import Data.Char

bytesToInt :: Int -> Int -> String -> Int
bytesToInt 0 a _ = a
bytesToInt a b (x:xs) = bytesToInt (a - 1) ((b * 128) + (ord x)) xs
bytesToInt _ _ _ = 0

getAddresse :: String -> Int
getAddresse (x:xs)
    | ord x > 63 = bytesToInt ((ord x) - 64) 0 xs
    | otherwise = bytesToInt (ord x) 0 xs
getAddresse _ = 0

moveToAddresse :: Int -> String -> String
moveToAddresse 0 str = str
moveToAddresse a (_:xs) = moveToAddresse (a - 1) xs
moveToAddresse _ _ = []

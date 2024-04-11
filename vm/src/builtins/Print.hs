module Print
    ( hydPrint
    ) where

import Data.Char

hydPrint :: [ String ] -> IO ()
hydPrint ((x:y:z:a:b:xs):xxs)
    | ord x == 126 = print (((ord y) * 2097152) + ((ord z) * 16384) + ((ord a) * 128) + (ord b)) >> hydPrint xxs
    | otherwise = putStrLn (x:y:z:a:b:xs) >> hydPrint xxs
hydPrint [] = return ()
hydPrint (x:xs) = putStrLn x  >> hydPrint xs

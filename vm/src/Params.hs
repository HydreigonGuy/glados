module Params
    ( getParamsList, giveFunctionParams
    ) where

import Data.Char

import Memory
import Add
import Sub
import Mul
import Div
import Mod
import Greater
import Lesser
import Equal
import If

trimStr :: Int -> String -> String
trimStr 0 str = str
trimStr x (_:xs) = trimStr (x - 1) xs
trimStr _ _ = []

getParam :: Int -> String -> String
getParam 0 _ = []
getParam a (x:xs) = x:(getParam (a - 1) xs)
getParam _ _ = []

runGetParam :: Int -> String -> String -> String
runGetParam a (x:y:z:zz:xs) d = case (ord x, ord y, ord z, ord zz) of
    (13, 14, 15, _) -> evalReturn (zz:xs) d
    (_, 13, 14, 15) -> evalReturn xs d
    _ -> getParam a (y:z:zz:xs)
runGetParam a (x:xs) _ = getParam a xs

getPointerParam :: String -> String -> String
getPointerParam a d = case (moveToAddresse (getAddresse a) d) of
    (x:xs)
        | (ord x) == 126 -> (((toEnum 126 :: Char)):(runGetParam 4 (x:xs) d))
        | (ord x) > 63 -> getPointerParam (x:xs) d
        | otherwise -> (runGetParam (ord x) (x:xs) d)
    _ -> []

getParamsList :: String -> String -> [ String ]
getParamsList (x:xs) d
    | ord x == 0 = []
    | ord x == 126 = (((toEnum 126 :: Char)):(runGetParam 4 (x:xs) d)):(getParamsList (trimStr 4 xs) d)
    | ord x > 63 = (getPointerParam (x:xs) d):(getParamsList (trimStr ((ord x) - 64) xs) d)
    | otherwise = (runGetParam (ord x) (x:xs) d):(getParamsList (trimStr (ord x) xs) d)
getParamsList _ _ = []


transposeArgs :: String -> String -> String
transposeArgs (x:xs) (y:ys)
    | ord x == 0 = y:ys
    | x == y = x:(getParam (ord x) xs) ++ (transposeArgs (trimStr (ord x) xs) (trimStr (ord x) ys))
    | ((ord x) - 64) == (ord y) = x:(getParam (ord x) xs) ++ (transposeArgs (trimStr (ord x) xs) (trimStr (ord x) ys))
    | otherwise = []
transposeArgs _ str = str

giveFunctionParams :: String -> String -> String
giveFunctionParams (x:xs) d
    | ord x == 0 = d
    | otherwise = (getParam (getAddresse (x:xs)) d) ++ (transposeArgs (trimStr (ord x) xs) (trimStr (getAddresse (x:xs)) d))


skipArgs :: String -> Int -> String
skipArgs (a:xs) 0
    | ord a == 0 = xs
    | ord a > 63 = skipArgs xs ((ord a) - 64)
    | ord a == 126 = skipArgs xs (4)
    | otherwise = skipArgs xs (ord a)
skipArgs (a:xs) b = skipArgs xs (b - 1)
skipArgs _ _ = []

evalReturn :: String -> String -> String
evalReturn ('i':'f':xs) d = hydIf (getParamsList xs d)
evalReturn ('=':xs) d = hydEqual (getParamsList xs d)
evalReturn ('+':xs) d = hydAdd (getParamsList xs d)
evalReturn ('-':xs) d = hydSub (getParamsList xs d)
evalReturn ('*':xs) d = hydMul (getParamsList xs d)
evalReturn ('/':xs) d = hydDiv (getParamsList xs d)
evalReturn ('m':'o':'d':xs) d = hydMod (getParamsList xs d)
evalReturn ('>':xs) d = hydGreater (getParamsList xs d)
evalReturn ('<':xs) d = hydLesser (getParamsList xs d)
evalReturn str d = runEvalReturn (moveToAddresse (getAddresse str) d) d

returnString :: Int -> String -> String
returnString 0 _ = []
returnString a (x:xs) = x:(returnString (a - 1) xs)
returnString _ _ = []

getReturnString :: String -> String
getReturnString (x:xs)
    | ord x == 0 = [(toEnum 0 :: Char)]
    | otherwise = returnString ((ord x) + 1) (x:xs)
getReturnString _ = [(toEnum 0 :: Char)]

runEvalReturn :: String -> String -> String
runEvalReturn (x:y:z:xs) str = case (ord x, ord y, ord z) of
    (127, 10, 13) -> getReturnString xs
    (13, 14, 15) -> evalReturn xs str >> runEvalReturn (skipArgs xs 0) str
    _ -> evalReturn (y:z:xs) str
runEvalReturn _ _ = [(toEnum 0 :: Char)]

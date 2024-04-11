module Eval
    ( runEval
    ) where

import Data.Char

import Memory
import Params
import Print

skipArgs :: String -> Int -> String
skipArgs ('p':'r':'i':'n':'t':xs) 0 = skipArgs xs 0
skipArgs (a:xs) 0
    | ord a == 0 = xs
    | ord a == 126 = skipArgs xs 4
    | ord a > 63 = skipArgs xs ((ord a) - 64)
    | otherwise = skipArgs xs (ord a)
skipArgs (a:xs) b = skipArgs xs (b - 1)
skipArgs _ _ = []

callFunction :: String -> String -> IO ()
callFunction ('p':'r':'i':'n':'t':xs) d = hydPrint (getParamsList xs d)
callFunction ('+':xs) d = return ()
callFunction ('-':xs) d = return ()
callFunction ('*':xs) d = return ()
callFunction ('/':xs) d = return ()
callFunction ('=':xs) d = return ()
callFunction ('>':xs) d = return ()
callFunction ('<':xs) d = return ()
callFunction ('m':'o':'d':xs) d = return ()
callFunction ('i':'f':xs) d = return ()
callFunction f d = eval (moveToAddresse (getAddresse f) d) (giveFunctionParams f d)

eval :: String -> String -> IO ()
eval (x:y:z:xs) str = case (ord x, ord y, ord z) of
    (127, 10, 13) -> return ()
    (13, 14, 15) -> callFunction xs str >> case xs of
        ('+':ys) -> eval (skipArgs ys 0) str
        ('-':ys) -> eval (skipArgs ys 0) str
        ('*':ys) -> eval (skipArgs ys 0) str
        ('/':ys) -> eval (skipArgs ys 0) str
        ('=':ys) -> eval (skipArgs ys 0) str
        ('>':ys) -> eval (skipArgs ys 0) str
        ('<':ys) -> eval (skipArgs ys 0) str
        ('m':'o':'d':ys) -> eval (skipArgs ys 0) str
        ('i':'f':ys) -> eval (skipArgs ys 0) str
        (_) -> eval (skipArgs xs 0) str
    _ -> eval (y:z:xs) str
eval _ _ = return ()

runEval :: String -> String -> IO ()
runEval ('m':'a':'i':'n':xs) d = eval xs d
runEval (x:xs) d = runEval xs d
runEval _ _ = putStrLn "No main function found."

module Read (
  readThenHandleFile
) where

import System.IO
import Data.Char
import Eval

checkFormat :: String -> IO ()
checkFormat (x:'H':'Y':'D':xs)
  | x == (toEnum 127 :: Char) = runEval xs xs
  | otherwise = putStrLn "Incorrect file format!"
checkFormat _ = putStrLn "Incorrect file format!"

readThenHandleFile :: String -> IO ()
readThenHandleFile file_name = do
    x <- readFile file_name
    checkFormat x

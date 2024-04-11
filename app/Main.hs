module Main (main) where

import Repl(nTtyManager)
import System.Environment(getArgs)
import Args

main :: IO ()
main = do
    args <- getArgs
    case handleArgs args of
        (_, []) -> putStrLn "Please select a file to compile!"
        (output, files) -> nTtyManager (mapM readFile files) output
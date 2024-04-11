module Main (main) where

import Read
import Help

import System.Environment(getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--help":xs) -> help
        ("-h":xs) -> help
        [file_name] -> readThenHandleFile file_name
        _ -> help

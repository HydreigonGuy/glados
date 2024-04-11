module Repl (
    nTtyManager
) where

import System.Exit(exitWith, ExitCode(..))

import Ast
import Eval

import Cpt
import Parser

compileCmds :: [ Cpt ] -> [ Ast ] -> Int -> Maybe String
compileCmds (x:xs) defs p = case cptToAST x of
    Just (Define "main" y z) -> case evalAST (Define "main" y z) defs (p + 4) of
        Just (Define a b _) -> case compileCmds xs (Define a b p:defs) (p + length (astToString (Define a b p))) of
            Just str -> Just (astToString (Define a b p) ++ str)
            _ -> Nothing
        Just a -> case compileCmds xs defs (p + length (astToString a)) of
            Just str -> Just (astToString a ++ str)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
    Just y -> case evalAST y defs p of
        Just (Define a b _) -> case compileCmds xs (Define a b p:defs) (p + length (astToString (Define a b p))) of
            Just str -> Just (astToString (Define a b p) ++ str)
            _ -> Nothing
        Just a -> case compileCmds xs defs (p + length (astToString a)) of
            Just str -> Just (astToString a ++ str)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
compileCmds _ _ _ = Just []

execCmd :: String -> Maybe String
execCmd cmd = case parseStringToCpt cmd of
    Left x -> Nothing
    Right x -> compileCmds x [] 0

nTtyManager :: IO [String] -> String -> IO()
nTtyManager func _ = do
    str <- func
    case execCmd (foldl1 (++) str) of
        Nothing -> putStrLn "ERROR *** Couldn't evaluate this file"
            >> exitWith (ExitFailure 84)
        Just d -> writeFile "output.hyd" ((toEnum 127 :: Char):'H':'Y':'D':d)

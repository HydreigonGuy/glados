module Main (main) where

import Test.Hspec

import AstSpec (cptAstSpec)
import EvalSpec (evalSpec)
import ParserSpec (parserSpec)

main :: IO ()
main = do
    hspec cptAstSpec
    hspec evalSpec
    hspec parserSpec

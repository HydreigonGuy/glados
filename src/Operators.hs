module Operators
    ( myMul, myAdd, mySub, myDiv, myMod, myEq, myLess
    ) where

import Ast

myMul :: Maybe Ast -> Maybe Ast -> Maybe Ast
myMul (Just (ANumber a)) (Just (ANumber b)) = Just (ANumber (a * b))
myMul _ _ = Nothing

myAdd :: Maybe Ast -> Maybe Ast -> Maybe Ast
myAdd (Just (ANumber a)) (Just (ANumber b)) = Just (ANumber (a + b))
myAdd _ _ = Nothing

mySub :: Maybe Ast -> Maybe Ast -> Maybe Ast
mySub (Just (ANumber a)) (Just (ANumber b)) = Just (ANumber (a - b))
mySub _ _ = Nothing

myDiv :: Maybe Ast -> Maybe Ast -> Maybe Ast
myDiv _ (Just (ANumber 0)) = Nothing
myDiv (Just (ANumber a)) (Just (ANumber b)) = Just (ANumber (a `div` b))
myDiv _ _ = Nothing

myMod :: Maybe Ast -> Maybe Ast -> Maybe Ast
myMod (Just (ANumber a)) (Just (ANumber b)) = Just (ANumber (a `mod` b))
myMod _ _ = Nothing

myLess :: Maybe Ast -> Maybe Ast -> Maybe Ast
myLess (Just (ANumber a)) (Just (ANumber b)) = Just (ABool (a < b))
myLess _ _ = Nothing

myEq :: Maybe Ast -> Maybe Ast -> Maybe Ast
myEq (Just (ANumber a)) (Just (ANumber b)) = Just (ABool (a == b))
myEq (Just (ABool a)) (Just (ABool b)) = Just (ABool (a == b))
myEq _ _ = Nothing

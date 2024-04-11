module Eval
    ( evalAST
    ) where

import Ast

getSymbolValue :: String -> [ Ast ] -> Maybe Ast
getSymbolValue str ((Define symbol _ pointer):xs)
    | str == symbol = Just (APointer pointer)
    | otherwise = getSymbolValue str xs
getSymbolValue str (_:xs) = getSymbolValue str xs
getSymbolValue _ [] = Nothing

returnIfValue :: Maybe Ast -> Ast -> Ast -> [Ast] -> Int -> Maybe Ast
returnIfValue (Just (ABool False)) _ falseRes d p = evalAST falseRes d p
returnIfValue (Just (ABool True)) trueRes _ d p = evalAST trueRes d p
returnIfValue (Just (ANumber _)) trueRes _ d p = evalAST trueRes d p
returnIfValue _ _ _ _ _ = Nothing

evalAST :: Ast -> [ Ast ] -> Int -> Maybe Ast
evalAST (ANumber i) _ _ = Just (ANumber i)
evalAST (ABool i) _ _ = Just (ABool i)
evalAST (AChar i) _ _ = Just (AChar i)
evalAST (ALength i) _ _ = Just (ALength i)
evalAST (FunctionEnd) _ _ = Just (FunctionEnd)
evalAST (AArray []) _ _ = Just (AArray [])
evalAST (AArray (x:(ASymbol "+"):y:xs)) d p = case evalAST (Call "+" [x, y]) d p of
    Just (z) -> case evalAST (AArray xs) (d) (p + (length (astToString z))) of
        Just (AArray zz) -> Just (AArray (z:zz))
        _ -> Nothing
    _ -> Nothing
evalAST (AArray (x:(ASymbol "-"):y:xs)) d p = case evalAST (Call "-" [x, y]) d p of
    Just (z) -> case evalAST (AArray xs) (d) (p + (length (astToString z))) of
        Just (AArray zz) -> Just (AArray (z:zz))
        _ -> Nothing
    _ -> Nothing
evalAST (AArray (x:(ASymbol "*"):y:xs)) d p = case evalAST (Call "*" [x, y]) d p of
    Just (z) -> case evalAST (AArray xs) (d) (p + (length (astToString z))) of
        Just (AArray zz) -> Just (AArray (z:zz))
        _ -> Nothing
    _ -> Nothing
evalAST (AArray (x:(ASymbol "/"):y:xs)) d p = case evalAST (Call "/" [x, y]) d p of
    Just (z) -> case evalAST (AArray xs) (d) (p + (length (astToString z))) of
        Just (AArray zz) -> Just (AArray (z:zz))
        _ -> Nothing
    _ -> Nothing
evalAST (AArray (x:(ASymbol "mod"):y:xs)) d p = case evalAST (Call "mod" [x, y]) d p of
    Just (z) -> case evalAST (AArray xs) (d) (p + (length (astToString z))) of
        Just (AArray zz) -> Just (AArray (z:zz))
        _ -> Nothing
    _ -> Nothing
evalAST (AArray (x:xs)) d p = case evalAST x d p of
    Just (Define a b c) -> case evalAST (AArray xs) ((Define a b p):d) (p + (length (astToString (Define a b p)))) of
        Just (AArray z) -> Just (AArray ((Define a b p):z))
        _ -> Nothing
    Just (a) -> case evalAST (AArray xs) d (p + (length (astToString a))) of
        Just (AArray z) -> Just (AArray (a:z))
        _ -> Nothing
    _ -> Nothing
evalAST (ASymbol ('$':a)) d _ = Just (ASymbol ('$':a))
evalAST (ASymbol s) d _ = getSymbolValue s d
evalAST (Call "return" [x]) d p = case evalAST x d p of
    Just (y) -> Just (Call "return" [y])
    _ -> Nothing
evalAST (Call "*" [x, y]) d p = case (evalAST x d p, evalAST y d p) of
    (Just (z), Just (zz)) -> Just (Call "*" [z, zz])
    _ -> Nothing
evalAST (Call "+" [x, y]) d p = case (evalAST x d p, evalAST y d p) of
    (Just (z), Just (zz)) -> Just (Call "+" [z, zz])
    _ -> Nothing
evalAST (Call "-" [x, y]) d p = case (evalAST x d p, evalAST y d p) of
    (Just (z), Just (zz)) -> Just (Call "-" [z, zz])
    _ -> Nothing
evalAST (Call "/" [x, y]) d p = case (evalAST x d p, evalAST y d p) of
    (Just (z), Just (zz)) -> Just (Call "/" [z, zz])
    _ -> Nothing
evalAST (Call "mod" [x, y]) d p = case (evalAST x d p, evalAST y d p) of
    (Just (z), Just (zz)) -> Just (Call "mod" [z, zz])
    _ -> Nothing
evalAST (Call "<" [x, y]) d p = case (evalAST x d p, evalAST y d p) of
    (Just (z), Just (zz)) -> Just (Call "<" [z, zz])
    _ -> Nothing
evalAST (Call ">" [x, y]) d p = case (evalAST x d p, evalAST y d p) of
    (Just (z), Just (zz)) -> Just (Call ">" [z, zz])
    _ -> Nothing
evalAST (Call "eq?" [x, y]) d p = case (evalAST x d p, evalAST y d p) of
    (Just (z), Just (zz)) -> Just (Call "=" [z, zz])
    _ -> Nothing
evalAST (Call "print" x) d p = case evalAST (AArray x) d p of
    Just (AArray y) -> Just (Call "print" y)
    _ -> Nothing
evalAST (Call "if" [condition, trueRes, falseRes]) d p = case (evalAST trueRes d p, evalAST falseRes d p) of
    (Just (t), Just (f)) -> returnIfValue (evalAST condition d p) t f d p
    _ -> Nothing
--evalAST (Call "car" [AArray ((ASymbol (x:_)):_)]) _ = Just (AChar x)
--evalAST (Call "car" [AArray (x:_)]) d = evalAST x d
--evalAST (Call "car" [a]) d = case evalAST a d of
--    Just (AArray ((ASymbol (x:_)):_)) -> Just (AChar x)
--    Just (AArray (x:_)) -> evalAST x d
--    _ -> Nothing
--evalAST (Call "cdr" [AArray ((ASymbol (_:xs)):ys)]) _ = Just (AArray (ASymbol xs:ys))
--evalAST (Call "cdr" [AArray (_:xs)]) _ = Just (AArray xs)
--evalAST (Call "cdr" [x]) d = case evalAST x d of
--    Just (AArray ((ASymbol (_:xs)):ys)) -> Just (AArray (ASymbol xs:ys))
--    Just (AArray (_:xs)) -> Just (AArray xs)
--    _ -> Nothing
--evalAST (Call "cons" [x, AArray arr]) _ = Just (AArray (x:arr))
--evalAST (Call "cons" [x, y]) d = case evalAST y d of
--    Just (AArray arr) -> Just (AArray (x:arr))
--    _ -> Nothing
--evalAST (Call "list" (x:xs)) d = case evalAST (Call "list" xs) d of
--    Just (AArray list) -> Just (AArray (x:list))
--    _ -> Nothing
--evalAST (Call "list" []) _ = Just (AArray [])
--evalAST (Call "append" []) _ = Just (AArray [])
--evalAST (Call "append" (AArray a:xs)) d = case evalAST (Call "append" xs) d of
--    Just (AArray b) -> Just (AArray (a ++ b))
--    _ -> Nothing
--evalAST (Call "append" (x:xs)) d = case evalAST x d of
--    Just (AArray a) -> case evalAST (Call "append" xs) d of
--        Just (AArray b) -> Just (AArray (a ++ b))
--        _ -> Nothing
--    _ -> Nothing
evalAST (Call f args) d p = case getSymbolValue f d of
    Just (APointer p) ->case evalAST (AArray args) d p of
        Just (AArray ll) -> Just (FunctionCall p ll)
        _ -> Nothing
    _ -> Nothing
evalAST (Define x y z) d p = case evalAST (AArray y) d p of
    Just (AArray ll) -> Just (Define x ll z)
    _ -> Nothing
evalAST _ _ _ = Nothing

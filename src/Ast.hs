module Ast
    ( Ast(..),
    cptToAST, astToString
    ) where

import Cpt

data Ast = Define { aSymbol::String, aAst::[Ast], aPointer::Int }
        | FunctionCall { aPointer::Int, aArg::[Ast] }
        | Call { aSymbol::String, aArg::[Ast] }
        | CallIf { aCondition::Ast, aTrue::[Ast], aFalse::[Ast] }
        | ASymbol String
        | ANumber Int
        | ABool Bool
        | APointer Int
        | AArray [Ast]
        | AChar Char
        | ALength Char
        | FunctionEnd
        deriving (Show, Eq)

defineFunction :: [Cpt] -> [Cpt] -> Maybe [Ast]
defineFunction _ [] = Just [FunctionEnd, ALength '\0']
defineFunction [] (Instruction [Symbol "return"]:xs) = (++) [FunctionEnd, ALength '\0'] <$> defineFunction [] xs
defineFunction [] (Instruction (Symbol "return":nxt):xs) = traverse cptToAST nxt
    >>= (\x -> (++) (FunctionEnd : x) <$> defineFunction [] xs)
defineFunction [] (x:xs) = ((:) <$> cptToAST x) <*> defineFunction [] xs
defineFunction args list = (++) (ALength (toEnum (length args) :: Char) :
    replicate (length args) (ANumber 0)) <$> defineFunction [] list

cptToAST :: Cpt -> Maybe Ast
cptToAST (Number i) = Just (ANumber i)
cptToAST (Symbol i) = Just (ASymbol i)
cptToAST (Boolean i) = Just (ABool i)
cptToAST (Array i) = Just AArray <*> traverse cptToAST i
cptToAST (List _) = Nothing
cptToAST (Argument _) = Nothing
cptToAST (Function [Symbol name, Argument args, List its]) = Just (Define name) <*> defineFunction args its <*> Just 0
cptToAST (Function _) = Nothing
cptToAST (Instruction [Symbol a, Symbol "=", b, Symbol c, d]) = case (cptToAST b, cptToAST d) of
    (Just (x), Just (y)) -> Just (Define a [Call c [x, y]] 0)
    _ -> Nothing
cptToAST (Instruction (Symbol a:Symbol "=":b)) = Just (Define a) <*> traverse cptToAST b <*> Just 0
cptToAST (Instruction [a, Symbol "+", b]) = Just (Call "+") <*> traverse cptToAST [a, b]
cptToAST (Instruction [a, Symbol "-", b]) = Just (Call "-") <*> traverse cptToAST [a, b]
cptToAST (Instruction [a, Symbol "*", b]) = Just (Call "*") <*> traverse cptToAST [a, b]
cptToAST (Instruction [a, Symbol "/", b]) = Just (Call "/") <*> traverse cptToAST [a, b]
cptToAST (Instruction [a, Symbol "%", b]) = Just (Call "mod") <*> traverse cptToAST [a, b]
cptToAST (Instruction [Symbol "if", Condition c, List it, Symbol "else", List ie]) = Just CallIf <*>
    cptToAST (Condition c) <*> (init <$> defineFunction [] it) <*> (init <$> defineFunction [] ie)
cptToAST (Instruction (Symbol x:list)) = Just (Call x) <*> traverse cptToAST list
cptToAST (Instruction _) = Nothing
cptToAST (Condition [a, Symbol "=", b]) = Just (Call "eq?") <*> traverse cptToAST [a, b]
cptToAST (Condition [a, Symbol "<", b]) = Just (Call "<") <*> traverse cptToAST [a, b]
cptToAST (Condition [a, Symbol ">", b]) = Just (Call ">") <*> traverse cptToAST [a, b]
cptToAST (Condition [a, Symbol "+", b]) = Just (Call "+") <*> traverse cptToAST [a, b]
cptToAST (Condition [a, Symbol "-", b]) = Just (Call "-") <*> traverse cptToAST [a, b]
cptToAST (Condition [a, Symbol "*", b]) = Just (Call "*") <*> traverse cptToAST [a, b]
cptToAST (Condition [a, Symbol "/", b]) = Just (Call "/") <*> traverse cptToAST [a, b]
cptToAST (Condition [a, Symbol "%", b]) = Just (Call "mod") <*> traverse cptToAST [a, b]
cptToAST (Condition _) = Nothing

getPointerString :: Int -> String
getPointerString x
    | x < 128 = [(toEnum 65 :: Char),(toEnum (x) :: Char)]
    | x < 16384 = [(toEnum 66 :: Char),(toEnum (x `div` 16384) :: Char),(toEnum (x `mod` 16384) :: Char)]
    | x < 2097152 = [(toEnum 67 :: Char),(toEnum (x `div` 2097152) :: Char),(toEnum ((x `mod` 2097152) `div` 16384) :: Char),(toEnum (x `mod` 16384) :: Char)]
    | otherwise = [(toEnum 68 :: Char),(toEnum (x `div` 268435456) :: Char),(toEnum ((x `mod` 268435456) `div` 2097152) :: Char),(toEnum ((x `mod` 2097152) `div` 16384) :: Char),(toEnum (x `mod` 16384) :: Char)]

astToString :: Ast -> String
astToString (Define "main" a _) = "main" ++ (foldl1 (++) (map astToString a))
astToString (Define _ a _) = foldl1 (++) (map astToString a)
astToString (ALength a) = [a]
astToString (APointer a) = getPointerString a
astToString (ANumber a) = [(toEnum 126 :: Char),(toEnum (a `div` 268435456) :: Char), (toEnum ((a `mod` 268435456) `div` 16384) :: Char), (toEnum ((a `mod` 16384) `div` 128) :: Char), (toEnum (a `mod` 128) :: Char)]
astToString (ABool True) = [(toEnum 1 :: Char),(toEnum 1 :: Char)]
astToString (ABool False) = [(toEnum 1 :: Char),(toEnum 0 :: Char)]
astToString (FunctionCall p l) = [(toEnum 13 :: Char),(toEnum 14 :: Char),(toEnum 15 :: Char)] ++ (astToString (APointer p)) ++ (concat (traverse astToString l)) ++ [(toEnum 0 :: Char)]
astToString (Call str l) = [(toEnum 13 :: Char),(toEnum 14 :: Char),(toEnum 15 :: Char)] ++ str ++ (foldl1 (++) (map astToString l)) ++ [(toEnum 0 :: Char)]
astToString (ASymbol ('$':str)) = (toEnum (length str) :: Char):str ++ [(toEnum 0 :: Char)]
astToString (ASymbol str) = (toEnum (length str) :: Char):str ++ [(toEnum 0 :: Char)]
astToString (AChar c) = [(toEnum 13 :: Char), c]
astToString (FunctionEnd) = [(toEnum 127 :: Char),(toEnum 10 :: Char),(toEnum 13 :: Char)]
astToString _ = []

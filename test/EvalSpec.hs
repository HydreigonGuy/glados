module EvalSpec (
    evalSpec
) where

import Test.Hspec

import Eval (evalAST)
import Ast (Ast(..))

evalSpec :: Spec
evalSpec =
    describe "Evaluating Ast" $ do
        describe "Basic tests" $ do

            it "just a number" $ do
                (evalAST (ANumber 42) [] 0) `shouldBe` Just (ANumber 42)

            it "just a negative number" $ do
                (evalAST (ANumber (-8)) [] 0) `shouldBe` Just (ANumber (-8))

            it "just a bool" $ do
                (evalAST (ABool True) [] 0) `shouldBe` Just (ABool True)

            it "just a another bool" $ do
                (evalAST (ABool False) [] 0) `shouldBe` Just (ABool False)

            it "just a char" $ do
                (evalAST (AChar 'a') [] 0) `shouldBe` Just (AChar 'a')

            it "just a MAJ char" $ do
                (evalAST (AChar 'Z') [] 0) `shouldBe` Just (AChar 'Z')
        
            it "just a empty array" $ do
                (evalAST (AArray []) [] 0) `shouldBe` Just (AArray [])

            it "just a array with a single number" $ do
                (evalAST (AArray [ANumber 5]) [] 0) `shouldBe` Just (AArray [ANumber 5])

            it "just a array with a single negative number" $ do
                (evalAST (AArray [ANumber (-5)]) [] 0) `shouldBe` Just (AArray [ANumber (-5)])

            let defineSymbol = Define "x" [ANumber 5] 0
            it "just a array with a define symbol" $ do
                (evalAST (AArray [defineSymbol]) [] 0) `shouldBe` Just (AArray [Define {aSymbol = "x", aAst = [ANumber 5], aPointer = 0}])

            let defineSymbol = Define "x" [ANumber (-5)] 0
            it "just a array with a define symbol (negative)" $ do
                (evalAST (AArray [defineSymbol]) [] 0) `shouldBe` Just (AArray [Define {aSymbol = "x", aAst = [ANumber (-5)], aPointer = 0}])

            let symbolArray = [ANumber 5, ASymbol "+", ANumber 3]
            it "just a array with multiple symbols" $ do
                (evalAST (AArray symbolArray) [] 0) `shouldBe` Just (AArray [Call {aSymbol = "+", aArg = [ANumber 5,ANumber 3]}])

            let symbolArray = [ANumber (-5), ASymbol "+", ANumber (-3)]
            it "just a array with multiple symbols (negative)" $ do
                (evalAST (AArray symbolArray) [] 0) `shouldBe` Just (AArray [Call {aSymbol = "+", aArg = [ANumber (-5),ANumber (-3)]}])

            let symbolArray = [ANumber 4 , ASymbol "-", ANumber 2]
            it "just a array with multiple symbols" $ do
                (evalAST (AArray symbolArray) [] 0) `shouldBe` Just (AArray [Call {aSymbol = "-", aArg = [ANumber 4,ANumber 2]}])

            let symbolArray = [ANumber (-4) , ASymbol "-", ANumber (-2)]
            it "just a array with multiple symbols (negative)" $ do
                (evalAST (AArray symbolArray) [] 0) `shouldBe` Just (AArray [Call {aSymbol = "-", aArg = [ANumber (-4),ANumber (-2)]}])

            let symbolArray = [ANumber 5, ASymbol "*", ANumber 5]
            it "just a array with multiple symbols" $ do
                (evalAST (AArray symbolArray) [] 0) `shouldBe` Just (AArray [Call {aSymbol = "*", aArg = [ANumber 5,ANumber 5]}])

            let symbolArray = [ANumber (-5), ASymbol "*", ANumber (-5)]
            it "just a array with multiple symbols (negative)" $ do
                (evalAST (AArray symbolArray) [] 0) `shouldBe` Just (AArray [Call {aSymbol = "*", aArg = [ANumber (-5),ANumber (-5)]}])

            let symbolArray = [ANumber 25, ASymbol "/", ANumber 5]
            it "just a array with multiple symbols" $ do
                (evalAST (AArray symbolArray) [] 0) `shouldBe` Just (AArray [Call {aSymbol = "/", aArg = [ANumber 25,ANumber 5]}])

            let symbolArray = [ANumber (-25), ASymbol "/", ANumber (-5)]
            it "just a array with multiple symbols (negative)" $ do
                (evalAST (AArray symbolArray) [] 0) `shouldBe` Just (AArray [Call {aSymbol = "/", aArg = [ANumber (-25),ANumber (-5)]}])

            let symbolArray = [ANumber 6, ASymbol "mod", ANumber 2]
            it "just a array with multiple symbols" $ do
                (evalAST (AArray symbolArray) [] 0) `shouldBe` Just (AArray [Call {aSymbol = "mod", aArg = [ANumber 6,ANumber 2]}])

            let symbolArray = [ANumber (-6), ASymbol "mod", ANumber (-2)]
            it "just a array with multiple symbols (negative)" $ do
                (evalAST (AArray symbolArray) [] 0) `shouldBe` Just (AArray [Call {aSymbol = "mod", aArg = [ANumber (-6),ANumber (-2)]}])

            let functionCall = FunctionCall 0 []
            it "just a array with a invalid function call" $ do
                (evalAST (AArray [functionCall]) [Define "f" [] 0, FunctionEnd] 0) `shouldBe` Nothing

            let function = Define "f" [ANumber 5] 1
            let functionCall = FunctionCall 1 []
            it "just a array with a valid function call" $ do
                (evalAST (AArray [functionCall]) [function, FunctionEnd]) 0 `shouldBe` Nothing

        describe "Function call tests" $ do
            it "return" $ do
                (evalAST (Call "return" [ANumber 5]) [] 0) `shouldBe` Just (Call "return" [ANumber 5])

            it "return (negative)" $ do
                (evalAST (Call "return" [ANumber (-5)]) [] 0) `shouldBe` Just (Call "return" [ANumber (-5)])

            it "simple multiplication" $ do
                (evalAST (Call "*" [ANumber 3, ANumber 5]) [] 0) `shouldBe` Just (Call {aSymbol = "*", aArg = [ANumber 3,ANumber 5]})

            it "simple multiplication (negative)" $ do
                (evalAST (Call "*" [ANumber (-3), ANumber (-5)]) [] 0) `shouldBe` Just (Call {aSymbol = "*", aArg = [ANumber (-3),ANumber (-5)]})

            it "simple addition" $ do
                (evalAST (Call "+" [ANumber 3, ANumber 5]) [] 0) `shouldBe` Just (Call {aSymbol = "+", aArg = [ANumber 3,ANumber 5]})

            it "simple addition (negative)" $ do
                (evalAST (Call "+" [ANumber (-3), ANumber (-5)]) [] 0) `shouldBe` Just (Call {aSymbol = "+", aArg = [ANumber (-3),ANumber (-5)]})

            it "simple subtraction" $ do
                (evalAST (Call "-" [ANumber 3, ANumber 5]) [] 0) `shouldBe` Just (Call {aSymbol = "-", aArg = [ANumber 3,ANumber 5]})

            it "simple subtraction (negative)" $ do
                (evalAST (Call "-" [ANumber (-3), ANumber (-5)]) [] 0) `shouldBe` Just (Call {aSymbol = "-", aArg = [ANumber (-3),ANumber (-5)]})

            it "simple division" $ do
                (evalAST (Call "/" [ANumber 10, ANumber 2]) [] 0) `shouldBe` Just (Call {aSymbol = "/", aArg = [ANumber 10,ANumber 2]})

            it "simple division (negative)" $ do
                (evalAST (Call "/" [ANumber (-10), ANumber (-2)]) [] 0) `shouldBe` Just (Call {aSymbol = "/", aArg = [ANumber (-10),ANumber (-2)]})

            it "modulus" $ do
                (evalAST (Call "mod" [ANumber 10, ANumber 3]) [] 0) `shouldBe` Just (Call {aSymbol = "mod", aArg = [ANumber 10,ANumber 3]})

            it "modulus (negative)" $ do
                (evalAST (Call "mod" [ANumber (-10), ANumber (-3)]) [] 0) `shouldBe` Just (Call {aSymbol = "mod", aArg = [ANumber (-10),ANumber (-3)]})

            it "less than" $ do
                (evalAST (Call "<" [ANumber 3, ANumber 5]) [] 0) `shouldBe` Just (Call {aSymbol = "<", aArg = [ANumber 3,ANumber 5]})

            it "less than (negative)" $ do
                (evalAST (Call "<" [ANumber (-3), ANumber (-5)]) [] 0) `shouldBe` Just (Call {aSymbol = "<", aArg = [ANumber (-3),ANumber (-5)]})

            it "greater than" $ do
                (evalAST (Call ">" [ANumber 3, ANumber 5]) [] 0) `shouldBe` Just (Call {aSymbol = ">", aArg = [ANumber 3,ANumber 5]})

            it "greater than (negative)" $ do
                (evalAST (Call ">" [ANumber (-3), ANumber (-5)]) [] 0) `shouldBe` Just (Call {aSymbol = ">", aArg = [ANumber (-3),ANumber (-5)]})

            it "equal than" $ do
                (evalAST (Call "eq?" [ANumber 3, ANumber 5]) [] 0) `shouldBe` Just (Call {aSymbol = "=", aArg = [ANumber 3,ANumber 5]})

            it "equal than (negative)" $ do
                (evalAST (Call "eq?" [ANumber (-3), ANumber (-5)]) [] 0) `shouldBe` Just (Call {aSymbol = "=", aArg = [ANumber (-3),ANumber (-5)]})

            it "print" $ do
                (evalAST (Call "print" [ANumber 3]) [] 0) `shouldBe` Just (Call {aSymbol = "print", aArg = [ANumber 3]})

            it "print (negative)" $ do
                (evalAST (Call "print" [ANumber (-3)]) [] 0) `shouldBe` Just (Call {aSymbol = "print", aArg = [ANumber (-3)]})

            
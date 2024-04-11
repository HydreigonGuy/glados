module AstSpec (
    cptAstSpec
) where

import Test.Hspec

import Ast (Ast(..), cptToAST)
import Cpt (Cpt(..))

cptAstSpec :: Spec
cptAstSpec =
    describe "Cpt to Ast conversion" $ do
        describe "Good Input" $ do

            it "return a Ast ANumber when given a Cpt Number" $ do
                cptToAST (Number 10) `shouldBe` Just (ANumber 10)

            it "return a Ast ASymbol when given a Cpt Symbol" $ do
                cptToAST (Symbol "define") `shouldBe` Just (ASymbol "define")

            it "return a True Ast ABool when given a Cpt Boolean" $ do
                cptToAST (Boolean True) `shouldBe` Just (ABool True)

            it "return a False Ast ABool when given a Cpt Boolean" $ do
                cptToAST (Boolean False) `shouldBe` Just (ABool False)

            it "return a call of add" $ do
                cptToAST (Instruction [Symbol "a",Symbol "+",Symbol "b"]) `shouldBe` Just (Call {aSymbol = "+", aArg = [ASymbol "a",ASymbol "b"]})

            it "return a call of sub" $ do
                cptToAST (Instruction [Symbol "a",Symbol "-",Symbol "b"]) `shouldBe` Just (Call {aSymbol = "-", aArg = [ASymbol "a",ASymbol "b"]})

            it "return a call of mul" $ do
                cptToAST (Instruction [Symbol "a",Symbol "*",Symbol "b"]) `shouldBe` Just (Call {aSymbol = "*", aArg = [ASymbol "a",ASymbol "b"]})

            it "return a call of div" $ do
                cptToAST (Instruction [Symbol "a",Symbol "/",Symbol "b"]) `shouldBe` Just (Call {aSymbol = "/", aArg = [ASymbol "a",ASymbol "b"]})

            it "return a call of mod" $ do
                cptToAST (Instruction [Symbol "a",Symbol "%",Symbol "b"]) `shouldBe` Just (Call {aSymbol = "mod", aArg = [ASymbol "a",ASymbol "b"]})

            it "return a call of assignation" $ do
                cptToAST (Instruction [Symbol "a",Symbol "=",Symbol "b"]) `shouldBe` Just Define {aSymbol = "a", aAst = [ASymbol "b"], aPointer = 0}

            it "return a bool for =" $ do
                cptToAST (Condition [Symbol "ok",Symbol "=",Number 1]) `shouldBe` Just (Call {aSymbol = "eq?", aArg = [ASymbol "ok",ANumber 1]})

            it "return a bool for <" $ do
                cptToAST (Condition [Symbol "ok",Symbol "<",Number 1]) `shouldBe` Just (Call {aSymbol = "<", aArg = [ASymbol "ok",ANumber 1]})

            it "return a bool for >" $ do
                cptToAST (Condition [Symbol "ok",Symbol ">",Number 1]) `shouldBe` Just (Call {aSymbol = ">", aArg = [ASymbol "ok",ANumber 1]})

        describe "Bad Input" $ do

            it "return Nothing when not given enough Cpt to create Define" $ do
                cptToAST (List [Symbol "define", Symbol "x"]) `shouldBe` Nothing

            it "return Nothing when not given a Symbol to create Define" $ do
                cptToAST (List [Symbol "define", Number 3]) `shouldBe` Nothing

            it "empty list" $ do
                cptToAST (List []) `shouldBe` Nothing


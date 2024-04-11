module ParserSpec (
    parserSpec
) where

import Test.Hspec

import Cpt (Cpt(..))
import Parser (runParser,
  parseChar,
  parseAnyChar,
  parseOr,
  parseAnd,
  parseAndWith,
  parseMany,
  parseSome,
  parseUInt,
  parseInt,
  parseSpace,
  parseNumber,
  parseSymbol,
  parseBoolean,
  parseList)

parserSpec :: Spec
parserSpec =
    describe "Evaluating Parser" $ do
        describe "Basic functions" $ do
            describe "Test parseChar function" $ do
                it "Test 01" $ do
                    runParser (parseChar 'a') "abcd" `shouldBe` Just ('a',"bcd")

                it "Test 02" $ do
                    runParser (parseChar 'z') "abcd" `shouldBe` Nothing

                it "Test 03" $ do
                    runParser (parseChar 'b') "abcd" `shouldBe` Nothing

                it "Test 04" $ do
                    runParser (parseChar 'a') "aaaa" `shouldBe` Just ('a',"aaa")

            describe "Test parseAnyChar function" $ do    
                it "Test 01" $ do
                    runParser (parseAnyChar "bca") "abcd" `shouldBe` Just ('a',"bcd")

                it "Test 02" $ do
                    runParser (parseAnyChar "xyz") "abcd" `shouldBe` Nothing

                it "Test 03" $ do
                    runParser (parseAnyChar "bca") "cdef" `shouldBe` Just ('c',"def")
 
            describe "Test parseOr function" $ do
                it "Test 01" $ do
                    runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd" `shouldBe` Just ('a',"bcd")

                it "Test 02" $ do
                    runParser (parseOr (parseChar 'a') (parseChar 'b')) "bcda" `shouldBe` Just ('b',"cda")

                it "Test 03" $ do
                    runParser (parseOr (parseChar 'a') (parseChar 'b')) "xyz" `shouldBe` Nothing

            describe "Test parseAnd function" $ do
                it "Test 01" $ do
                    runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd" `shouldBe` Just (('a','b'),"cd")

                it "Test 02" $ do
                    runParser (parseAnd (parseChar 'a') (parseChar 'b')) "bcda" `shouldBe` Nothing

                it "Test 03" $ do
                    runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acd" `shouldBe` Nothing

            describe "Test parseAndWith function" $ do
                it "Test 01" $ do
                    runParser (parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "abcd" `shouldBe` Just ("ab","cd")

                it "Test 02" $ do
                    runParser (parseAndWith (\ x y -> [x,y]) (parseChar 'x') (parseChar 'z')) "abcd" `shouldBe` Nothing

                it "Test 03" $ do
                    runParser (parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'd')) "abcd" `shouldBe` Nothing

            describe "Test parseMany function" $ do
                it "Test 01" $ do
                    runParser (parseMany (parseChar ' ')) "   foobar" `shouldBe` Just ("   ","foobar")

                it "Test 02" $ do
                    runParser (parseMany (parseChar ' ')) "foobar   " `shouldBe` Just ("","foobar   ")

            describe "Test parseSome function" $ do
                it "Test 01" $ do
                    runParser (parseSome (parseAnyChar ['0'..'9'])) "42foobar" `shouldBe` Just ("42","foobar")

                it "Test 02" $ do
                    runParser (parseSome (parseAnyChar ['0'..'9'])) "foobar42" `shouldBe` Nothing

            describe "Test parseUInt function" $ do
                it "Test 01" $ do
                    runParser parseUInt "42 foobar" `shouldBe` Just (42," foobar")

                it "Test 02" $ do
                    runParser parseUInt "foobar42" `shouldBe` Nothing

            describe "Test parseInt function" $ do
                it "Test 01" $ do
                    runParser parseInt "-42" `shouldBe` Just (-42,"")

                it "Test 02" $ do
                    runParser parseInt "42" `shouldBe` Just (42,"")

                it "Test 03" $ do
                    runParser parseInt "foobar42" `shouldBe` Nothing

        describe "Basic CPT function" $ do
            describe "Test parseSpace function" $ do
                it "Test 01" $ do
                    runParser parseSpace " foo" `shouldBe` Just ((),"foo")

                it "Test 02" $ do
                    runParser parseSpace "          foo" `shouldBe` Just ((),"foo")

                it "Test 03" $ do
                    runParser parseSpace "foo " `shouldBe` Just ((),"foo ")

            describe "Test parseNumber function" $ do
                it "Test 01" $ do
                    runParser parseNumber "123" `shouldBe` Just (Number 123,"")

                it "Test 02" $ do
                    runParser parseNumber "123a" `shouldBe` Just (Number 123,"a")

                it "Test 03" $ do
                    runParser parseNumber "-123" `shouldBe` Just (Number (-123),"")

            describe "Test parseSymbol function" $ do
                it "Test 01" $ do
                    runParser parseSymbol "+" `shouldBe` Just (Symbol "+","")

                it "Test 02" $ do
                    runParser parseSymbol "#t" `shouldBe` Nothing

                it "Test 03" $ do
                    runParser parseSymbol "if" `shouldBe` Just (Symbol "if","")

            describe "Test parseBoolean function" $ do
                it "Test 01" $ do
                    runParser parseBoolean "#t" `shouldBe` Just (Boolean True,"")

                it "Test 02" $ do
                    runParser parseBoolean "#f" `shouldBe` Just (Boolean False,"")

                it "Test 03" $ do
                    runParser parseBoolean "t" `shouldBe` Nothing

            describe "Test parseList function" $ do
                it "Test 01" $ do
                    runParser parseList "(1 2 3)" `shouldBe` Just (List [Number 1,Number 2,Number 3],"")

                it "Test 02" $ do
                    runParser parseList "(a b c)" `shouldBe` Just (List [Symbol "a",Symbol "b",Symbol "c"],"")

                it "Test 03" $ do
                    runParser parseList "(a (b c) d)" `shouldBe` Just (List [Symbol "a",List [Symbol "b",Symbol "c"],Symbol "d"],"")

                it "Test 04" $ do
                    runParser parseList "(a (b c) d (+(1)))" `shouldBe` Just (List [Symbol "a",List [Symbol "b",Symbol "c"],Symbol "d",List [Symbol "+",List [Number 1]]],"")
                
                it "Test 06" $ do
                    runParser parseList "(   3  + (2 b (+ 3 a  (  +  )   )  )   )" `shouldBe` Just (List [Number 3,Symbol "+",List [Number 2,Symbol "b",List [Symbol "+",Number 3,Symbol "a",List [Symbol "+"]]]],"")
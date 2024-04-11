module Parser
(
  Parser (..),
  parseChar,
  parseAnyChar,
  parseOr,
  parseAnd,
  parseAndWith,
  parseMany,
  parseSome,
  parseUInt,
  parseInt,
  parseString,
  parseSpace,
  parseNumber,
  parseSymbol,
  parseBoolean,
  parseSomeSepBy,
  parseList,
  parseCpt,
  parseAll,
  parseRest,
  parseNext,
  parseStringToCpt
) where

import Cpt
import Control.Applicative
import Control.Monad (void)
import Data.Char()

newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
  fmap fct parser = Parser func where
    func str = case runParser parser str of
      Nothing -> Nothing
      Just (a, string) -> Just (fct a, string)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  p1 <|> p2 = Parser $ \str -> case runParser p1 str of
    Nothing -> runParser p2 str
    r -> r

instance Applicative Parser where
  pure a = Parser $ \str -> Just (a, str)
  p1 <*> p2 = Parser $ \str -> case runParser p1 str of
    Just (r1, str1) -> runParser (r1 <$> p2) str1
    Nothing -> Nothing

instance Monad Parser where
  return = pure
  p1 >>= f = Parser $ \str -> case runParser p1 str of
    Just (r1, xs) -> runParser (f r1) xs
    Nothing -> Nothing

parseChar :: Char -> Parser Char
parseChar c = Parser $ \s -> case s of
  (x:xs) | x == c -> Just (c, xs)
  _ -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar validChars = Parser $ \s -> case s of
  (x:xs) | x `elem` validChars -> Just (x, xs)
  _ -> Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser $ runParser (p1 <|> p2)

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser $ runParser (((,) <$> p1) <*> p2)

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f (Parser p1) (Parser p2) = Parser $ \s -> case p1 s of
  Just (r1, xs) -> case p2 xs of
    Just (r2, xr) -> Just (f r1 r2, xr)
    Nothing -> Nothing
  Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \s -> case runParser p s of
  Just (r, xs) -> runParser ((r:) <$> parseMany p) xs
  Nothing -> Just ([], s)

parseSome :: Parser a -> Parser [a]
parseSome p = Parser $ runParser (((:) <$> p) <*> parseMany p)

parseUInt :: Parser Int
parseUInt = Parser $ runParser (read <$> parseSome (parseAnyChar ['0'..'9']))

parseInt :: Parser Int
parseInt = Parser $ \s -> case runParser (parseChar '-') s of
  Just (_, rs) -> runParser (negate <$> parseUInt) rs
  Nothing -> runParser parseUInt s

parseString :: String -> Parser String
parseString str = Parser $ \s ->
    let prefixMatches = take (length str) s == str
        remainingInput = drop (length str) s
    in case prefixMatches of
        True -> Just (str, remainingInput)
        False -> Nothing

parseSpace :: Parser ()
parseSpace = void $ parseMany $ parseAnyChar " \n\t\r"

parseComment :: Parser ()
parseComment = void $ parseString "#|" *> (void $ parseMany $ parseAnyChar " \n\t\r") *> parseMany (parseAnyChar $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-*/?!@$%&_=<> '\n\t\r") *> void (parseString "|#") *> (void $ parseMany $ parseAnyChar " \n\t\r")

parseNumber :: Parser Cpt
parseNumber = Number <$> parseInt

parseSymbol :: Parser Cpt
parseSymbol = Symbol <$> parseSome (parseAnyChar $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-*/?!@$%&_=<>")

parseBoolean :: Parser Cpt
parseBoolean = Boolean <$> ((True <$ parseString "#t") <|> (False <$ parseString "#f"))

parseSomeSepBy :: Parser a -> Parser sep -> Parser [a]
parseSomeSepBy p sep = (:) <$> p <*> parseMany (sep *> p)

parseList :: Parser Cpt
parseList = List <$> (parseChar '(' *> parseSpace *> parseSomeSepBy parseCpt parseSpace <* parseSpace <* parseChar ')')

parseArrayList :: Parser Cpt
parseArrayList = Array <$> (parseChar '\'' *> parseChar '(' *> parseSpace *> parseSomeSepBy parseCpt parseSpace <* parseSpace <* parseChar ')')

parseArraySimple :: Parser Cpt
parseArraySimple = Array <$> (parseChar '\'' *> (pure <$> parseCpt) <* parseSpace)

parseRest :: (Cpt, String) -> Maybe ([Cpt], String)
parseRest (parsed, rest) = runParser ((parsed :) <$> parseNext) rest <|> Just ([parsed], rest)

parseNext :: Parser [Cpt]
parseNext = Parser parseAll

parseFunction :: Parser Cpt
parseFunction = do
  _ <- parseString "fct" <* parseSpace
  nameFct <- parseSymbol
  _ <- parseSpace <* parseChar '(' <* parseSpace
  argList <- parseSomeSepBy parseCpt parseSpace
  _ <- parseSpace <* parseChar ')' <* parseSpace <* parseChar '{' <* parseSpace
  body <- parseSomeSepBy parseTypo parseSpace
  _ <- parseSpace <* parseChar '}'
  return $ Function [nameFct, Argument argList, List body]

parseIfElse :: Parser Cpt
parseIfElse = do
  _ <- parseString "if" <* parseSpace <* parseChar '(' <* parseSpace
  cond <- parseSomeSepBy parseCpt parseSpace
  _ <- parseSpace <* parseChar ')' <* parseSpace <* parseChar '{' <* parseSpace
  thenExpr <- parseSomeSepBy parseTypo parseSpace
  _ <- parseSpace <* parseChar '}' <* parseSpace
  _ <- parseString "else" <* parseSpace <* parseChar '{' <* parseSpace
  elseExpr <- parseSomeSepBy parseTypo parseSpace
  _ <- parseSpace <* parseChar '}' <* parseSpace
  return $ Instruction [Symbol "if", Condition cond, List thenExpr, Symbol "else", List elseExpr]

parseIf :: Parser Cpt
parseIf = do
  _ <- parseString "if" <* parseSpace <* parseChar '(' <* parseSpace
  cond <- parseSomeSepBy parseCpt parseSpace
  _ <- parseSpace <* parseChar ')' <* parseSpace <* parseChar '{' <* parseSpace
  thenExpr <- parseTypo <* parseSpace <* parseChar '}' <* parseSpace
  return $ Instruction [Symbol "if", Condition cond, thenExpr]

parseStringSymbol :: Parser Cpt
parseStringSymbol = do
  first <- parseChar '$'
  str <- parseSome (parseAnyChar $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " +-*/?!@$%&_=<>")
  return $ Symbol (first:str)

parseInstruction :: Parser Cpt
parseInstruction = do
  body <- parseSomeSepBy parseCpt parseSpace
  _ <- parseSpace <* parseChar ';'
  return $ Instruction body

parseCpt :: Parser Cpt
parseCpt = parseNumber <|> parseArrayList <|> parseArraySimple <|> parseStringSymbol <|> parseSymbol <|> parseList

parseTypo :: Parser Cpt
parseTypo = (parseComment *> parseTypo) <|> parseFunction <|> parseIfElse <|> parseIf <|> parseInstruction

parseAll :: String -> Maybe ([Cpt], String)
parseAll s = runParser (parseSpace *> parseTypo <* parseSpace) s >>= parseRest

parseStringToCpt :: String -> Either String [Cpt]
parseStringToCpt s = case parseAll s of
    Just (parsed, _) -> Right parsed
    Nothing -> Left "*** ERROR : couldn't parse the given string."

module Main where

import Data.Void
import Data.String

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Expr = Var Char
          | FuncDef [Char] Expr
          | FuncAppl Expr Expr
          deriving (Show)

main :: IO ()
main = parse

parse :: IO ()
parse = parseTest expr "\\x.\\y.yx"

paren :: Parser a -> Parser a
paren = between (char '(') (char ')')

var :: Parser Expr
var = do
  ch <- lowerChar
  return $ Var ch

lambda :: Parser Expr
lambda = do
  _ <- char '\\' <|> char 'λ'
  ch <- many lowerChar
  _ <- char '.'
  ex <- expr
  return $ FuncDef ch ex

term :: Parser Expr
term = parens expr
  <|> lambda
  <|> var

expr :: Parser Expr
expr = do
  terms <- many term
  return $ 

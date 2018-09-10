module LambdaCalc.Parser
  ( mparse
  ) where

import Data.Void
import Data.String

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Expr = Var Char
          | FuncDef [Char] Expr
          | FuncAppl Expr Expr
          deriving (Show)

mparse :: IO ()
mparse = parseTest expr "(λx.λy.yx)(λx.x)"

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

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
  return $ foldl1 FuncAppl terms

module LambdaCalc.Parser
  ( mparse
  ) where

import Data.Void
import LambdaCalc.Types
import LambdaCalc.Simplify
import LambdaCalc.PrettyPrinter
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

var :: Parser Expr
var = Variable . Var <$> lowerChar

lambda :: Parser Expr
lambda = do
  _ <- char '\\' <|> char 'λ'
  ch <- many lowerChar
  _ <- char '.'
  ex <- expr
  return $ Def (Var <$> ch) ex

term :: Parser Expr
term = parens expr
  <|> lambda
  <|> var

expr :: Parser Expr
expr = foldl1 Appl <$> many term

mparse :: IO ()
mparse = case parse expr "" "(λx.λy.yx)(λx.x)z" of
  Left bundle -> putStr (errorBundlePretty bundle)
  Right xs -> do
    putStrLn . pprint $ xs
    putStrLn . pprint . betaReduce . betaReduce $ xs

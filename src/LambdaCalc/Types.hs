module LambdaCalc.Types where

newtype Var = Var {getVar :: Char}
            deriving Show

data Expr = Variable Var
          | Def [Var] Expr
          | Appl Expr Expr
          deriving Show

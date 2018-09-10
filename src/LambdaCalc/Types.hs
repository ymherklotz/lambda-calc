module LambdaCalc.Types where

data Expr = Var Char
          | FuncDef [Char] Expr
          | FuncAppl Expr Expr
          deriving (Show)

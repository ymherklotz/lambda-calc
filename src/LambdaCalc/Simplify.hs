module LambdaCalc.Simplify where

import LambdaCalc.Types

-- Reduces an expression using Normal order.
betaReduce :: Expr -> Expr
betaReduce (Appl (Def (x:xs) body) expr) = replaceExpr x expr body

replaceExpr :: Var -> Expr -> Expr -> Expr
replaceExpr var other (Variable dvar) =
  if var == dvar
  then other
  else (Variable dvar)
replaceExpr var other (Def b expr) = Def b $ replaceExpr var other expr
replaceExpr var other (Appl fun expr) = Appl (rep fun) $ rep expr
  where rep = replaceExpr var other

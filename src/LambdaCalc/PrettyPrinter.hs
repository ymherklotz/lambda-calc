module LambdaCalc.PrettyPrinter
  ( pprint
  ) where

import LambdaCalc.Types

pprint :: Expr -> String
pprint (Var var) = [var]
pprint (FuncDef bindings body) = "λ" ++ bindings ++ "." ++ pprint body
pprint (FuncAppl func body) = "(" ++ pprint func ++ ")(" ++ pprint body ++ ")"

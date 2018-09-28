module LambdaCalc.PrettyPrinter
  ( pprint
  ) where

import LambdaCalc.Types

pprint :: Expr -> String
pprint (Variable (Var var)) = [var]
pprint (Def bindings body) = "λ" ++ map getVar bindings ++ "." ++ pprint body
pprint (Appl func body) = "(" ++ pprint func ++ ")(" ++ pprint body ++ ")"

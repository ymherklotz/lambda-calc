module Main where

import LambdaCalc.Parser
  ( mparse
  )

import LambdaCalc.Types
import LambdaCalc.PrettyPrinter

main :: IO ()
main = putStrLn $ pprint (FuncAppl (FuncDef "x" (FuncDef "y" (FuncAppl (Var 'y') (Var 'x')))) (FuncDef "x" (Var 'x')))

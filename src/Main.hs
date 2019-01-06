module Main where

import LambdaCalc.Parser
import LambdaCalc.Simplify
import LambdaCalc.Types
import LambdaCalc.PrettyPrinter

main :: IO ()
main = mparse

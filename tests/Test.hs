{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Exit (exitFailure)
import Test.QuickCheck

prop_commutativeAdd :: Int -> Int -> Bool
prop_commutativeAdd x y = x + y == y + x

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  result <- runTests
  case result of
    True -> return ()
    _ -> exitFailure

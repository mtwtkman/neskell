module Main where

import Test.Tasty
import qualified Neskell.Test.InterpretTest as InterpretTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Neskell tests" [InterpretTest.tests]

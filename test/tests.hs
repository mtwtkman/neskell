module Main where

import qualified Neskell.Test.CPU.Opcode.LDATest as LDATest
import qualified Neskell.Test.InterpretTest as InterpretTest
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
  "Neskell tests"
  [InterpretTest.tests
  , LDATest.tests
  ]

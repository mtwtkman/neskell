module Main where

import qualified Neskell.Test.CPU.Opcode.LDATest as LDATest
import qualified Neskell.Test.CPU.Opcode.BRKTest as BRKTest
import qualified Neskell.Test.InterpretTest as InterpretTest
import qualified Neskell.Test.CPU.Opcode.TAXTest as TAXTest
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
  "Neskell tests"
  [InterpretTest.tests
  , LDATest.tests
  , BRKTest.tests
  , TAXTest.tests
  ]

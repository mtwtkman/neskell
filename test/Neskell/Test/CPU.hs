module Neskell.Test.CPU (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "CPU" [test_1]

test_1 :: TestTree
test_1 =
  testGroup
    "test_1"
    [testCase "" $ [1, 2, 3] `compare` [1, 2] @?= GT]

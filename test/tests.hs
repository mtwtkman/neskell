module Main where

import qualified Neskell.Test.CPU as CPUTest
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Neskell tests" [CPUTest.tests]

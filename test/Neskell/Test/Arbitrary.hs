{-# OPTIONS_GHC -Wno-orphans #-}

module Neskell.Test.Arbitrary where

import Neskell.CPU (CPU (CPU))
import Neskell.CPU.Register (Register (Register))
import Neskell.CPU.Register.ProcessorStatus (processorStatus)
import Test.QuickCheck (Arbitrary)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary))

instance Arbitrary Register where
  arbitrary =
    Register
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> return processorStatus

instance Arbitrary CPU where
  arbitrary =
    CPU
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

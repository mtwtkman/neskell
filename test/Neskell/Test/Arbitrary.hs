{-# OPTIONS_GHC -Wno-orphans #-}

module Neskell.Test.Arbitrary where

import qualified Data.Vector as V
import Data.Word (Word8)
import Neskell.CPU (CPU (CPU))
import Neskell.CPU.Register (Register (Register))
import Neskell.CPU.Register.ProcessorStatus (processorStatus)
import Test.QuickCheck (Arbitrary, choose, listOf)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary))
import Neskell.Memory (Memory (Memory))

instance Arbitrary Register where
  arbitrary =
    Register
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> return processorStatus

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> listOf arbitrary

instance Arbitrary Memory where
  arbitrary = Memory <$> arbitrary

instance Arbitrary CPU where
  arbitrary =
    CPU
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

newtype RegisterValue = RegisterValue Word8 deriving (Show, Eq)
instance Arbitrary RegisterValue where
  arbitrary = RegisterValue <$> choose (0, 255)

newtype NonZeroRegisterValue = NonZeroRegisterValue Word8 deriving (Show, Eq)
instance Arbitrary NonZeroRegisterValue where
  arbitrary = NonZeroRegisterValue <$> choose (1, 255)

newtype Bit7SetRegisterValue = Bit7SetRegisterValue Word8 deriving (Show, Eq)
instance Arbitrary Bit7SetRegisterValue where
  arbitrary = Bit7SetRegisterValue <$> choose (128, 255)

newtype Bit7UnsetRegisterValue = Bit7UnsetRegisterValue Word8 deriving (Show, Eq)
instance Arbitrary Bit7UnsetRegisterValue where
  arbitrary = Bit7UnsetRegisterValue <$> choose (0, 127)

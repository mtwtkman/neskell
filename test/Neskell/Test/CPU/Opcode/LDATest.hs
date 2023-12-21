{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Neskell.Test.CPU.Opcode.LDATest where

import Data.Bits ((.&.))
import Data.Word (Word8)
import Neskell.CPU (CPU (CPU, cpuRegister))
import Neskell.CPU.Opcode.LDA (immediate)
import Neskell.CPU.Register (Register (Register, regA, regPS))
import Neskell.CPU.Register.ProcessorStatus (
  ProcessorStatus (sN, sZ),
  processorStatus,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), NonZero (NonZero), choose, oneof, testProperty)

tests :: TestTree
tests =
  testGroup
    "LDA"
    [ prop_immediate
    ]

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

newtype AValue = AValue Word8 deriving (Show, Eq)
instance Arbitrary AValue where
  arbitrary = AValue <$> choose (0, 255)

sieve7Bit :: (Monad m) => (Word8 -> Bool) -> [m Word8]
sieve7Bit comp = foldr (\x a -> if comp $ x .&. 0b01000000 then a <> [return x] else a) [] [0 .. 255]

newtype Bit7SetAValue = Bit7SetAValue Word8 deriving (Show, Eq)
instance Arbitrary Bit7SetAValue where
  arbitrary = Bit7SetAValue <$> oneof (sieve7Bit (> 0))

newtype Bit7UnsetAValue = Bit7UnsetAValue Word8 deriving (Show, Eq)
instance Arbitrary Bit7UnsetAValue where
  arbitrary = Bit7UnsetAValue <$> oneof (sieve7Bit (== 0))

prop_immediate :: TestTree
prop_immediate =
  testGroup
    "immadiate mode"
    [ testGroup
        "Zero flag"
        [ testProperty "0 value causes setting Zero flag"
            $ \cpu -> zeroFlagTest cpu 0 == Right True
        , testProperty "non-0 value never affect Zero flag"
            $ \cpu (NonZero v) -> zeroFlagTest cpu v == Right False
        ]
    , testGroup
        "Negative flag"
        [ testProperty "set bit 7 causes setting Negative flag"
            $ \cpu (Bit7SetAValue v) -> negativeFlagTest cpu v == Right True
        , testProperty "unset bit 7 never affect Negative flag"
            $ \cpu (Bit7UnsetAValue v) -> negativeFlagTest cpu v == Right False
        ]
    , testProperty "load value"
        $ \cpu (AValue v) -> fmap (regA . cpuRegister) (immediate (cpu :: CPU) v) == Right v
    ]
 where
  zeroFlagTest cpu v = fmap (sZ . regPS . cpuRegister) (immediate (cpu :: CPU) v)
  negativeFlagTest cpu v = fmap (sN . regPS . cpuRegister) (immediate (cpu :: CPU) v)

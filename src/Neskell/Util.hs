module Neskell.Util where

import Data.Word (Word16)

word16ToInt :: Word16 -> Int
word16ToInt = read . show

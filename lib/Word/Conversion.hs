module Word.Conversion
    ( fromInteger
    ) where

import Prelude hiding (fromInteger)

import Data.Word (Word8)

fromInteger :: Integer -> [Word8]
fromInteger = reverse . go []
    where
        go acc 0 = acc
        go acc n = go (fromIntegral (n `mod` 256) : acc) (n `div` 256)

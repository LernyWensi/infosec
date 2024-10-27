module System.Random.Extra
    ( nBitInteger
    , nBitOddInteger
    , nBitPrime
    ) where

import Math (highLevelPrimalityTest, lowLevelPrimalityTest)
import System.Random.Stateful

nBitInteger :: (Integral a, UniformRange a) => a -> IO a
nBitInteger size = uniformRM range globalStdGen
    where
        range =
            ( (2 ^ (size - 1)) + 1
            , (2 ^ size) - 1
            )

nBitOddInteger :: (Integral a, UniformRange a) => a -> IO a
nBitOddInteger size = do
    candidate <- nBitInteger size
    pure $
        if even candidate
            then candidate + 1
            else candidate

nBitPrime :: (Integral a, UniformRange a) => a -> IO a
nBitPrime size = do
    candidate <- nBitOddInteger size
    if not $ lowLevelPrimalityTest candidate
        then nBitPrime size
        else
            if all (\_ -> highLevelPrimalityTest candidate 2) ([1 .. 20] :: [Int])
                then pure candidate
                else nBitPrime size

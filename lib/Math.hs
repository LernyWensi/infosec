module Math
    ( isPerfectSquare
    , extendedGCD
    , modularInverse
    , isCoprime
    , primes
    , lowLevelPrimalityTest
    , highLevelPrimalityTest
    , decomposeOddInteger
    , (^%)
    ) where

import Data.Map qualified as Map

import Data.List (foldl')

isPerfectSquare :: (Integral a) => a -> Bool
isPerfectSquare n = n == round (sqrt $ fromIntegral n :: Float) ^ (2 :: Int)

extendedGCD :: (Integral a) => a -> a -> (a, a, a)
extendedGCD a 0 = (abs a, signum a, 0)
extendedGCD a b = (abs gcd, y, x - (a `div` b) * y)
    where
        (gcd, x, y) = extendedGCD b (a `mod` b)

modularInverse :: (Integral a) => a -> a -> Maybe a
modularInverse a m
    | gcd == 1 = Just $ x `mod` m
    | otherwise = Nothing
    where
        (gcd, x, _) = extendedGCD a m

isCoprime :: (Integral a) => a -> a -> Bool
isCoprime a b = gcd == 1
    where
        (gcd, _, _) = extendedGCD a b

infixr 8 ^%
(^%) :: (Integral a) => a -> a -> a -> a
(^%) _ 0 _ = 1
(^%) b e m
    | even e = (b' * b') `mod` m
    | otherwise = (b * b' * b') `mod` m
    where
        b' = b ^% (e `div` 2) $ m

primes :: (Integral a) => [a] -> [a]
primes xs = primes' xs Map.empty
    where
        primes' [] _ = []
        primes' (x : xs) table =
            case Map.lookup x table of
                Nothing -> x : primes' xs (Map.insert (x * x) [x] table)
                Just facts -> primes' xs (foldl' reinsert (Map.delete x table) facts)
            where
                reinsert table prime = Map.insertWith (++) (x + prime) [prime] table

lowLevelPrimalityTest :: (Integral a) => a -> Bool
lowLevelPrimalityTest = go (primes [2 .. 100])
    where
        go [] _ = True
        go (divisor : rest) candidate
            | (candidate `mod` divisor == 0) && (divisor ^ (2 :: Int) <= candidate) = False
            | otherwise = go rest candidate

highLevelPrimalityTest :: (Integral a) => a -> a -> Bool
highLevelPrimalityTest n a = base == 1 || test base (fst fact) 0 n
    where
        fact = decomposeOddInteger (n - 1)
        base = a ^% snd fact $ n
        test b s r n
            | r == s = b == n - 1
            | b == 1 && r /= 0 = False
            | otherwise = b == n - 1 || test (b ^% 2 $ n) s (r + 1) n

decomposeOddInteger :: (Integral a) => a -> (a, a)
decomposeOddInteger 0 = (0, 0)
decomposeOddInteger n = go 0 n
    where
        go s d
            | even d = go (s + 1) (d `div` 2)
            | otherwise = (s, d)

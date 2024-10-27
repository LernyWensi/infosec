module Labs.DES
    ( encode
    , decode
    ) where

import Data.Bits qualified as B

import Data.Bifunctor (bimap)
import Data.Char (chr, ord)
import Data.List.Extra (splitAtHalf, switchHalves)

type Bit = Bool
type Bits = [Bit]
type Permutation = [Int]

vowels :: String
vowels = "aeiouyAEIOUY"

encode :: String -> String -> [Int]
encode plain key = boolsToDecimal . encodeBlock key1 key2 <$> bits
    where
        bits = decimalToBools 8 . ord <$> plain
        key' = flip notElem vowels <$> key
        key1 = generateKey1 key'
        key2 = generateKey2 key'

decode :: [Int] -> String -> String
decode plain key = chr . boolsToDecimal . encodeBlock key1 key2 <$> bits
    where
        bits = decimalToBools 8 <$> plain
        key' = flip notElem vowels <$> key
        key1 = generateKey2 key'
        key2 = generateKey1 key'

encodeBlock :: Bits -> Bits -> Bits -> Bits
encodeBlock key1 key2 block = permutateInitial' secondHalf
    where
        firstHalf = mainMutation (permutateInitial block) key1
        secondHalf = mainMutation (switchHalves firstHalf) key2

        initialPermutation = [2, 6, 3, 1, 4, 8, 5, 7]
        permutateInitial = permutate initialPermutation
        permutateInitial' = permutate' initialPermutation

        mainMutation bits key = zipWith B.xor firstHalf mutatedSecondHalf <> secondHalf
            where
                (firstHalf, secondHalf) = splitAtHalf bits

                xoredSecondHalf = zipWith B.xor (permutate [4, 1, 2, 3, 2, 3, 4, 1] secondHalf) key
                mutatedSecondHalf =
                    permutate4
                        . uncurry (<>)
                        . bimap (getPair blockS0) (getPair blockS1)
                        $ splitAtHalf xoredSecondHalf

                permutate4 = permutate [2, 4, 3, 1]

                getPair encodeMatrix [bit1, bit2, bit3, bit4] =
                    decimalToBools 2 $
                        encodeMatrix
                            !! boolsToDecimal [bit1, bit4]
                            !! boolsToDecimal [bit2, bit3]

                blockS0 =
                    [ [1, 0, 3, 2]
                    , [3, 2, 1, 0]
                    , [0, 2, 1, 3]
                    , [3, 1, 3, 1]
                    ]

                blockS1 =
                    [ [1, 1, 2, 3]
                    , [2, 0, 1, 3]
                    , [3, 0, 1, 0]
                    , [2, 1, 0, 3]
                    ]

generateKey1 :: Bits -> Bits
generateKey1 = permutate8 . shiftLeft1 . permutate10

generateKey2 :: Bits -> Bits
generateKey2 = permutate8 . shiftLeft2 . shiftLeft1 . permutate10

permutate8 :: Bits -> Bits
permutate8 = permutate [6, 3, 7, 4, 8, 5, 10, 9]

permutate10 :: Bits -> Bits
permutate10 = permutate [3, 5, 2, 7, 4, 10, 1, 9, 8, 6]

shiftLeft1 :: Bits -> Bits
shiftLeft1 = mutateHalfs (permutate [2, 3, 4, 5, 1])

shiftLeft2 :: Bits -> Bits
shiftLeft2 = mutateHalfs (permutate [3, 4, 5, 1, 2])

mutateHalfs :: (Bits -> Bits) -> Bits -> Bits
mutateHalfs mutation bits = uncurry (<>) mutatedHalfs
    where
        mutatedHalfs = bimap mutation mutation $ splitAtHalf bits

permutate :: Permutation -> Bits -> Bits
permutate permutation bits = [bits !! (i - 1) | i <- permutation]

permutate' :: Permutation -> Bits -> Bits
permutate' permutation = permutate permutation'
    where
        permutation' =
            [ head [index' | (index', mutation) <- zip [1 ..] permutation, mutation == index]
            | index <- [1 .. length permutation]
            ]

boolsToDecimal :: [Bool] -> Int
boolsToDecimal bits = sum [if bit then 2 ^ index else 0 | (bit, index) <- bits']
    where
        bits' = zip (reverse bits) ([0 ..] :: [Int])

decimalToBools :: Int -> Int -> [Bool]
decimalToBools padding 0 = replicate (padding - 1) False <> [False]
decimalToBools padding decimal = replicate (padding - length bools) False <> bools
    where
        bools = reverse (convert decimal)

        convert 0 = []
        convert decimal = (remainder == 1) : convert quotient
            where
                (quotient, remainder) = decimal `divMod` 2

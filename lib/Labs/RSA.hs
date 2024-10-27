module Labs.RSA
    ( encode
    , decode
    , generateKeys
    ) where

import Data.ByteString.Conversion qualified as BSC

import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Math (isCoprime, modularInverse, (^%))
import System.Random.Extra (nBitPrime)

newtype PublickKey = PublickKey (Integer, Integer) deriving (Show)
newtype PrivateKey = PrivateKey (Integer, Integer) deriving (Show)

encode :: ByteString -> PublickKey -> Integer
encode bs (PublickKey (e, n)) = (m ^ e) `mod` n
    where
        m = BSC.toInteger bs

decode :: Integer -> PrivateKey -> ByteString
decode c (PrivateKey (d, n)) = BSC.fromInteger m
    where
        m = c ^% d $ n

generateKeys :: IO (PublickKey, PrivateKey)
generateKeys = go <&> getKeys
    where
        e = 65537
        bits = 1024 :: Integer

        getKeys (d, n) = (PublickKey (e, n), PrivateKey (d, n))

        go = do
            p <- nBitPrime (bits - 1)
            q <- nBitPrime (bits + 1)

            let n = p * q
                phi = (p - 1) * (q - 1)

            if isCoprime e phi
                then pure (fromMaybe (error "I fucked up.") $ modularInverse e phi, n)
                else go

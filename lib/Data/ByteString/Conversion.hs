module Data.ByteString.Conversion
    ( toInteger
    , fromInteger
    ) where

import Prelude hiding (fromInteger, toInteger)

import Data.ByteString qualified as BS
import Word.Conversion qualified as WC

import Data.Bits ((.<<.), (.|.))
import Data.ByteString (ByteString)

toInteger :: ByteString -> Integer
toInteger = foldr foldByte 0 . BS.unpack
    where
        foldByte byte integer = (integer .<<. 8) .|. fromIntegral byte

fromInteger :: Integer -> ByteString
fromInteger = BS.pack . WC.fromInteger

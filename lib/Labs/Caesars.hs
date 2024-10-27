module Labs.Caesars
    ( encode
    , decode
    ) where

import Data.Char (chr, ord)
import Numeric.Extra (wrapTop)

encode :: Int -> String -> String
encode offset = fmap $ chr . wrapTop 0x110000 . (+ offset) . ord

decode :: Int -> String -> String
decode offset = encode $ negate offset

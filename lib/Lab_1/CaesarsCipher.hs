module Lab_1.CaesarsCipher (encode, decode) where

import Data.Char (chr, ord)
import Utils (wrap)

decode :: Int -> String -> String
decode offset = encode $ negate offset

encode :: Int -> String -> String
encode offset = map $ chr . wrap 0x110000 . (+ offset) . ord

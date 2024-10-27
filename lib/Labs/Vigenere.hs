module Labs.Vigenere
    ( encode
    , decode
    ) where

import Data.Char (toLower)
import Data.Functor ((<&>))
import Data.List (elemIndex)

data Op = Decode | Encode

alphabet :: String
alphabet = ['a' .. 'z']

encode :: String -> String -> Maybe String
encode = encode' Encode

decode :: String -> String -> Maybe String
decode = encode' Decode

encode' :: Op -> String -> String -> Maybe String
encode' op string key
    | null string = Nothing
    | null key = Nothing
    | otherwise = indicies <&> fmap (alphabet !!)
    where
        indicies = mapM (getIndex op) . zip string $ intoValid string key

intoValid :: String -> String -> String
intoValid string key
    | length key >= length string = take (length string) key
    | otherwise = intoValid string $ key <> key

getIndex :: Op -> (Char, Char) -> Maybe Int
getIndex op (a, b) = do
    ai <- toLower a `elemIndex` alphabet
    bi <- toLower b `elemIndex` alphabet
    let result = case op of
            Decode -> ai - bi
            Encode -> ai + bi
    pure $ result `mod` length alphabet

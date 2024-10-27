module Labs.Playfair
    ( encode
    , decode
    ) where

import Data.Map qualified as Map

import Control.Applicative ((<|>))
import Data.Bifoldable (biList)
import Data.Char (isAlpha, toLower)
import Data.Functor ((<&>))
import Data.List (elemIndex, nub)
import Data.List.Extra (chunk)
import Numeric.Extra (wrapTop)

filler :: Char
filler = 'x'

alphabet :: String
alphabet = ['a' .. 'z']

replacements :: Map.Map Char Char
replacements = Map.fromList [('i', 'j')]

tableWidth :: Int
tableWidth = 5

encode :: String -> String -> Maybe String
encode = encode' 1

decode :: String -> String -> Maybe String
decode = encode' (-1)

encode' :: Int -> String -> String -> Maybe String
encode' offset string key = do
    table <- intoTable key
    pairs <- intoPairs <$> intoValid string
    encodedPairs <- mapM (encodePair table offset) pairs
    pure $ concatMap biList encodedPairs

encodePair :: [String] -> Int -> (Char, Char) -> Maybe (Char, Char)
encodePair table offset (a, b) = do
    coordsA <- getCoords table a
    coordsB <- getCoords table b
    pure $ applyRules table offset (coordsA, coordsB)

applyRules :: [String] -> Int -> ((Int, Int), (Int, Int)) -> (Char, Char)
applyRules table offset (a@(ax, ay), b@(bx, by))
    | ay == by = (getIncX a, getIncX b)
    | ax == bx = (getIncY a, getIncY b)
    | otherwise = (get (bx, ay), get (ax, by))
    where
        get (x, y) = table !! y !! x
        getIncY (x, y) = get (x, wrapTop' (y + offset))
        getIncX (x, y) = get (wrapTop' (x + offset), y)

getCoords :: [String] -> Char -> Maybe (Int, Int)
getCoords table char = foldr getCoords' Nothing indexedRows
    where
        indexedRows = zip ([0 ..] :: [Int]) table
        getCoords' (y, row) coords = coords <|> (,y) <$> char `elemIndex` row

intoTable :: String -> Maybe [String]
intoTable [] = Nothing
intoTable key = intoValid (key <> alphabet) <&> chunk' . nub

intoPairs :: String -> [(Char, Char)]
intoPairs [] = []
intoPairs [a] = [(a, filler)]
intoPairs (a : b : rest)
    | a == b = (a, filler) : intoPairs (b : rest)
    | otherwise = (a, b) : intoPairs rest

intoValid :: String -> Maybe String
intoValid string = case filter isAlpha string of
    [] -> Nothing
    valid -> pure $ replaceExtraChars . toLower <$> valid

replaceExtraChars :: Char -> Char
replaceExtraChars char = Map.findWithDefault char char replacements

wrapTop' :: Int -> Int
wrapTop' = wrapTop tableWidth

chunk' :: String -> [String]
chunk' = chunk tableWidth

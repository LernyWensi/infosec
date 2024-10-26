module Lab_1.PlayfairCipher (encode, decode) where

import Control.Arrow ((***))
import Data.Char (isAlpha, toLower)
import Data.List (elemIndex, nub)
import Data.Map qualified as Map
import Utils (chunk, tupleIntoString, wrap)

decode :: String -> String -> Maybe String
decode = encode' (-1)

encode :: String -> String -> Maybe String
encode = encode' 1

encode' :: Int -> String -> String -> Maybe String
encode' offset string keywoard = encoded >>= return . concatMap tupleIntoString
  where
    encoded = do
        table <- intoTable keywoard
        pairs <- intoPairs string

        let getCoords' = getCoords table
            applyRules' = applyRules table offset
            encodedPairs = applyRules' <$> (getCoords' *** getCoords') <$> pairs

        sequence encodedPairs

applyRules :: [String] -> Int -> (Maybe (Int, Int), Maybe (Int, Int)) -> Maybe (Char, Char)
applyRules table n (Just a@(ax, ay), Just b@(bx, by))
    | ay == by = Just (getIncX a, getIncX b)
    | ax == bx = Just (getIncY a, getIncY b)
    | otherwise = Just (get (bx, ay), get (ax, by))
  where
    get (x, y) = table !! y !! x
    getIncY (x, y) = get (x, wrap 5 (y + n))
    getIncX (x, y) = get (wrap 5 (x + n), y)
applyRules _ _ _ = Nothing

getCoords :: [String] -> Char -> Maybe (Int, Int)
getCoords table char = foldr getCoords' Nothing indexedRows
  where
    indexedRows = zip ([0 ..] :: [Int]) table

    getCoords' (y, row) acc = case acc of
        Just coords -> Just coords
        Nothing -> case char `elemIndex` row of
            Just x -> Just (x, y)
            Nothing -> Nothing

intoTable :: String -> Maybe [String]
intoTable keywoard = intoValid (keywoard <> ['a' .. 'z']) >>= return . chunk 5 . nub

intoPairs :: String -> Maybe [(Char, Char)]
intoPairs string = intoValid string >>= return . intoPairs'
  where
    filler = 'x'

    intoPairs' [] = []
    intoPairs' [x] = [(x, filler)]
    intoPairs' (x : y : xy) =
        if x == y
            then (x, filler) : intoPairs' (y : xy)
            else (x, y) : intoPairs' xy

intoValid :: String -> Maybe String
intoValid string = case replace . toLower <$> filter (isAlpha) string of
    [] -> Nothing
    valid -> Just valid
  where
    replacements = Map.fromList [('j', 'i')]
    replace char = Map.findWithDefault char char replacements

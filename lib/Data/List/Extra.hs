module Data.List.Extra
    ( chunk
    , splitAtHalf
    , switchHalves
    ) where

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk amount list = take amount list : chunk amount (drop amount list)

splitAtHalf :: [a] -> ([a], [a])
splitAtHalf list = splitAt (length list `div` 2) list

switchHalves :: [a] -> [a]
switchHalves list = uncurry (<>) $ splitAt half list
    where
        half = length list `div` 2

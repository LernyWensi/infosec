module Utils (chunk, wrap, tupleIntoString) where

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk amount list = take amount list : chunk amount (drop amount list)

wrap :: Int -> Int -> Int
wrap upperBound index =
    if wrapped < 0
        then wrapped + upperBound
        else wrapped
  where
    wrapped = index `mod` upperBound

tupleIntoString :: (Char, Char) -> String
tupleIntoString (a, b) = [a, b]

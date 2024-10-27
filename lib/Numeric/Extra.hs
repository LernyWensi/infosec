module Numeric.Extra
    ( wrapTop
    , intoOdd
    ) where

intoOdd :: (Integral a) => a -> a
intoOdd n
    | even n = n + 1
    | otherwise = n

wrapTop :: (Integral a) => a -> a -> a
wrapTop upperBound index =
    if wrapped < 0
        then wrapped + upperBound
        else wrapped
    where
        wrapped = index `mod` upperBound

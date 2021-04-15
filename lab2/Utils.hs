module Utils where

import Data.Bits

binPow :: Int -> Int -> Int -> Int
binPow _ _ 0 = 1
binPow n m p = do
  let v = binPow n m (shiftR p 1)
  if even p
    then mod (v * v) m
    else mod (v * v * n) m

chunksOf :: Int -> [a] -> [[a]]
chunksOf size l = do
  if length l < size
    then [l]
    else [take size l] ++ chunksOf size (drop size l)

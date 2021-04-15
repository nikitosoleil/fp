module DiscLog where

import Control.Parallel.Strategies
import Utils

data NamedFunction = NamedFunction
  { name :: String,
    calculate :: Int -> Int -> Int -> [Int]
  }

_discLogSimple :: Int -> Int -> Int -> [Int]
_discLogSimple n m a = filter (\x -> binPow n m x == a) [1 .. m]

_parallelPostprocess :: [(Int, Int)] -> Int -> [Int]
_parallelPostprocess l a = map snd (filter (\(v, k) -> v == a) l)

_discLogSimpleParallel :: Int -> Int -> Int -> [Int]
_discLogSimpleParallel n m = _parallelPostprocess (zip (parMap rdeepseq (binPow n m) [1 .. m]) [1 .. m])

_discLogBatchedParallel :: Int -> Int -> Int -> Int -> [Int]
_discLogBatchedParallel nThreads n m a = do
  let doBatch = \xl -> _parallelPostprocess (zip (map (binPow n m) xl) xl) a
  let chunkSize = div (m + nThreads - 1) nThreads
  let allBatches = chunksOf chunkSize [1 .. m]
  concat (parMap rdeepseq doBatch allBatches)


discLogSimple :: NamedFunction
discLogSimple = NamedFunction "discLogSimple" _discLogSimple

discLogSimpleParallel :: NamedFunction
discLogSimpleParallel = NamedFunction "discLogSimpleParallel" _discLogSimpleParallel

discLogBatchedParallel :: NamedFunction
discLogBatchedParallel = NamedFunction "discLogBatchedParallel" (_discLogBatchedParallel 12)
module Main where

import Data.List.Split
import Data.Time.Clock
import DiscLog
import Lib
import System.IO
import Utils

benchmark :: Int -> Int -> Int -> NamedFunction -> IO ()
benchmark n m a f = do
  putStrLn (name f)
  start <- getCurrentTime
  let mapped = calculate f n m a
  putStrLn ("Found solutions: " ++ show mapped)
  end <- getCurrentTime
  let time = end `diffUTCTime` start
  putStrLn ("Time took: " ++ show time)

main :: IO ()
main = do
  -- n ^ p == a (mod m)
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let inputs = map read (splitOn " " contents)

  let (n, p, m) = (inputs !! 0, inputs !! 1, inputs !! 2)
  --  let (n, p, m) = (101, 10007, 10000019)
  let a = binPow n m p

  putStrLn (show n ++ "^" ++ show p ++ " == " ++ show a ++ " (mod " ++ show m ++ ")")
  hClose handle

  mapM_ (benchmark n m a) [discLogBatchedParallel, discLogSimple, discLogSimpleParallel]

--  putStrLn "\ndiscLogBatchedParallel"
--  benchmark n m a (discLogBatchedParallel 12)
--
--  putStrLn "\ndiscLogSimple"
--  benchmark n m a (calculate discLogSimple)
--
--  putStrLn "\ndiscLogSimpleParallel"
--  benchmark n m a discLogSimpleParallel

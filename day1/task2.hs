module Main where
--------------------------------------------------------------------------------
import System.IO
import Data.List.Split
--------------------------------------------------------------------------------
createSlabs :: [Int] -> [Int] -> [Int]
createSlabs (x0:x1:x2:xs) slabs = do
  createSlabs (x1:x2:xs) (slabs ++ [x0+x1+x2])
createSlabs [x0,x1] slabs = slabs
createSlabs [x0] slabs = slabs
createSlabs [] slabs = slabs
--------------------------------------------------------------------------------
toInts :: [String] -> [Int]
toInts = map read
--------------------------------------------------------------------------------
countIncreases :: [Int] -> Int -> Int
countIncreases (i0:i1:is) cnt = do
  if i0 < i1
    then countIncreases (i1:is) (cnt + 1)
    else countIncreases (i1:is) cnt
countIncreases [_] cnt = cnt
countIncreases [] cnt = cnt
--------------------------------------------------------------------------------
main = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let
    ws = endBy "\n" content
    is = toInts ws
    slabs = createSlabs is []
    numIncreases = countIncreases slabs 0
  print numIncreases
  hClose handle

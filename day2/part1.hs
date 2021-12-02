module Main where
--------------------------------------------------------------------------------
import System.IO
--------------------------------------------------------------------------------
readInput = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let ws = (words content)
  hClose handle
  return ws
--------------------------------------------------------------------------------
main = do
  input <- readInput
  print (head input)
  --print (length content)
  --where
  --  content = readInput "input.txt"

module Main where
--------------------------------------------------------------------------------
import System.IO
--------------------------------------------------------------------------------
data DiveDirection = Forward | Up | Down | Unknown deriving Show
--------------------------------------------------------------------------------
parseDirection :: String -> DiveDirection
parseDirection str 
  | str == "forward" = Forward
  | str == "up"      = Up
  | str == "down"    = Down
  | otherwise        = Unknown
--------------------------------------------------------------------------------
parseInt :: String -> Int
parseInt str = read str
--------------------------------------------------------------------------------
parseDiveCommand :: String -> (DiveDirection, Int)
parseDiveCommand line = 
  (dir, num)
  where ws  = words line
        dir = parseDirection (ws!!0)
        num = parseInt       (ws!!1)
--------------------------------------------------------------------------------
parseDiveCommands :: String -> [(DiveDirection, Int)]
parseDiveCommands cmdsString = 
  cmds
  where ls   = lines cmdsString
        cmds = map parseDiveCommand ls
--------------------------------------------------------------------------------
dive :: (Int,Int) -> [(DiveDirection, Int)] -> (Int, Int)
dive (x,y) ((Forward, i):rest) = dive (x + i,y) rest 
dive (x,y) ((Up     , i):rest) = dive (x,y - i) rest 
dive (x,y) ((Down   , i):rest) = dive (x,y + i) rest 
dive (x,y) ((Unknown, i):rest) = dive (x,y) rest 
dive pos [] = pos
--------------------------------------------------------------------------------
main = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let startPos = (0,0)
      diveCmds = parseDiveCommands content
      endPos   = dive startPos diveCmds
  print (head diveCmds)
  print (length diveCmds)
  print endPos
  print ((fst endPos) * (snd endPos))
  hClose handle

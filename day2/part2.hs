module Main where
--------------------------------------------------------------------------------
import System.IO
--------------------------------------------------------------------------------
data DiveDirection = Forward | Up | Down deriving Show

--------------------------------------------------------------------------------
parseDiveDirection :: String -> Maybe DiveDirection
parseDiveDirection "forward" = Just Forward 
parseDiveDirection "up"      = Just Up 
parseDiveDirection "down"    = Just Down 
parseDiveDirection _         = Nothing 

--------------------------------------------------------------------------------
parseInt :: String -> Maybe Int
--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ----
parseInt s = return (read s)

--------------------------------------------------------------------------------
parseDiveCommand :: String -> Maybe(DiveDirection, Int)
--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ----
parseDiveCommand line = do
  dir <- parseDiveDirection (ws!!0)
  i   <- parseInt           (ws!!1)
  return (dir, i)
  where ws = words line

--------------------------------------------------------------------------------
parseDiveCommands  :: String -> Maybe [(DiveDirection, Int)]
parseDiveCommands' :: [String] -> Maybe [(DiveDirection, Int)]
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
parseDiveCommands cmdsString = parseDiveCommands' (lines cmdsString)
parseDiveCommands' (head:tail) = do
  cmd  <- parseDiveCommand head
  cmds <- parseDiveCommands' tail
  return ([cmd] ++ cmds)
--------------------------------------------------------------------------------
dive :: [Int] -> [(DiveDirection, Int)] -> [Int]
dive pos [] = pos
dive [horizontal,depth,aim] ((Forward, i):rest) =
  dive [horizontal + i, depth + i * aim, aim] rest
dive [horizontal,depth,aim] ((Up, i):rest) =
  dive [horizontal, depth, aim - i] rest
dive [horizontal,depth,aim] ((Down, i):rest) =
  dive [horizontal, depth, aim + i] rest
--------------------------------------------------------------------------------
main = do
  handle   <- openFile "input.txt" ReadMode
  content  <- hGetContents handle
  diveCmds <- parseDiveCommands content
  let endPos  = dive startPos diveCmds
  print endPos
  print ((endPos!!0) * (endPos!!1))
  hClose handle
  where
    horizontal = 0
    depth      = 0
    aim        = 0
    startPos   = [horizontal,depth,aim]

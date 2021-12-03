module AdventOfCode.Day2
  (DiveDirection(Forward, Up, Down, Unknown),
   parseDiveMoves) where
-- =============================================================================
import System.IO
import AdventOfCode
-- =============================================================================
--  print pos
  --let endPos  = dive startPos diveCmds
  --print endPos
  --print ((endPos!!0) * (endPos!!1))
-- =============================================================================
data DiveDirection = Forward | Up | Down | Unknown deriving Show
-- =============================================================================
parseDiveDirection :: String -> DiveDirection
parseDiveDirection "forward" = Forward 
parseDiveDirection "up"      = Up 
parseDiveDirection "down"    = Down 
parseDiveDirection _         = Unknown
-- =============================================================================
parseDiveMove :: String -> IO(DiveDirection, Int)
parseDiveMove line = do
  let dir = parseDiveDirection dirStr
      i   = read cntStr :: Int
  return (dir, i)
  where [dirStr, cntStr] = words line
-- =============================================================================
parseDiveMoves  :: String -> IO [(DiveDirection, Int)]
parseDiveMoves movesString = parseDiveMovesFromList (lines movesString)
-- =============================================================================
parseDiveMovesFromList :: [String] -> IO [(DiveDirection, Int)]
parseDiveMovesFromList [] = return []
parseDiveMovesFromList (head:tail) = do
  move  <- parseDiveMove head
  moves <- parseDiveMovesFromList tail
  return (move:moves)

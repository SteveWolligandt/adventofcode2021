module AdventOfCode.Day2.Part2 (part2) where
-- =============================================================================
import AdventOfCode.Day2
-- =============================================================================
dive :: [Int] -> [(DiveDirection, Int)] -> [Int]

dive pos [] = pos

dive [horizontal, depth, aim] ((Forward, i):moves) =
  dive [horizontal + i, depth + i * aim, aim] moves
  
dive [horizontal, depth, aim] ((Up, i):moves) =
  dive [horizontal, depth, aim - i] moves

dive [horizontal,depth,aim] ((Down, i):moves) =
  dive [horizontal, depth, aim + i] moves

dive [horizontal,depth,aim] ((Unknown, i):moves) =
  dive [horizontal, depth, aim] moves
--------------------------------------------------------------------------------
part2 :: String -> IO ()
part2 input = do
  diveCmds <- parseDiveMoves input
  let [horizontal, depth, aim] = dive startPos diveCmds
  print "Part 2 end position:"
  print [horizontal, depth, aim]
  print "Part 2 solution:"
  print (horizontal * depth)
  where
    horizontal = 0
    depth      = 0
    aim        = 0
    startPos   = [horizontal,depth,aim]

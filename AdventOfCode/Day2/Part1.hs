module AdventOfCode.Day2.Part1 (part1) where
-- =============================================================================
import AdventOfCode.Day2
-- =============================================================================
dive :: [Int] -> [(DiveDirection, Int)] -> [Int]
dive [horizontal,depth] ((Forward, i):rest) = dive [horizontal + i,depth] rest 
dive [horizontal,depth] ((Up     , i):rest) = dive [horizontal,depth - i] rest 
dive [horizontal,depth] ((Down   , i):rest) = dive [horizontal,depth + i] rest 
dive [horizontal,depth] ((Unknown, i):rest) = dive [horizontal,depth] rest 
dive pos [] = pos
--------------------------------------------------------------------------------
part1 :: String -> IO ()
part1 input = do
  diveCmds <- parseDiveMoves input
  let [horizontal, depth] = dive startPos diveCmds
  print "Part 1 end position:"
  print [horizontal, depth]
  print "Part 1 solution:"
  print (horizontal * depth)
  where
    horizontal = 0
    depth      = 0
    startPos   = [horizontal,depth]

module AdventOfCode.Lanternfish (simulateLanternfishes) where
import Data.List.Split
import Data.Word
--------------------------------------------------------------------------------
simulateLanternfishes :: [String] -> IO ()
simulateLanternfishes (str:_) = do
  putStrLn "Population after 80 days"
  print (countPopulation state80)
  putStrLn "Population after 256 days"
  print (countPopulation state256)
  where nums = parse str
        state0 = count nums (take 9 (repeat 0))
        state80 = simulate state0 80
        state256 = simulate state0 256
        countPopulation = foldl (+) 0
--------------------------------------------------------------------------------
-- Splits comma separated list of numbers
parse :: String -> [Word64]
parse numsString = map read(splitOn "," numsString)
--------------------------------------------------------------------------------
-- Simulates lantern fish population
simulate :: [Word64] -> Word64 -> [Word64]
simulate sim 0 = sim
simulate [a,b,c,d,e,f,g,h,i] numDays = simulate [b,c,d,e,f,g,h+a,i,a] (numDays-1)
--------------------------------------------------------------------------------
-- transforms list of fish counters to counts of counters
count :: [Word64] -> [Word64] -> [Word64]
count [] cnts = cnts
count (x:xs) [a,b,c,d,e,f,g,h,i] 
  | x == 0    = count xs [a+1,b,c,d,e,f,g,h,i]
  | x == 1    = count xs [a,b+1,c,d,e,f,g,h,i]
  | x == 2    = count xs [a,b,c+1,d,e,f,g,h,i]
  | x == 3    = count xs [a,b,c,d+1,e,f,g,h,i]
  | x == 4    = count xs [a,b,c,d,e+1,f,g,h,i]
  | x == 5    = count xs [a,b,c,d,e,f+1,g,h,i]
  | x == 6    = count xs [a,b,c,d,e,f,g+1,h,i]
  | x == 7    = count xs [a,b,c,d,e,f,g,h+1,i]
  | x == 8    = count xs [a,b,c,d,e,f,g,h,i+1]
  | otherwise = count xs [a,b,c,d,e,f,g,h,i]

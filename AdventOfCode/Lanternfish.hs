module AdventOfCode.Lanternfish (simulateLanternfishes) where
import Data.List.Split
import Data.Word
--------------------------------------------------------------------------------
simulateLanternfishes :: [String] -> IO ()
simulateLanternfishes (str:_) = do
  print (foldl (+) 0 state80)
  print (foldl (+) 0 state256)
  where nums = parse str
        state0 = count nums (take 9 (repeat 0))
        state80 = simulate state0 80
        state256 = simulate state0 256
--------------------------------------------------------------------------------
parse :: String -> [Word64]
parse numsString = map read(splitOn "," numsString)
--------------------------------------------------------------------------------
simulate :: [Word64] -> Word64 -> [Word64]
simulate sim 0 = sim
simulate [a,b,c,d,e,f,g,h,i] n = simulate [b,c,d,e,f,g,h+a,i,a] (n-1)
--------------------------------------------------------------------------------
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

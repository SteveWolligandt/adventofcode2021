module AdventOfCode.Day3 where
-- =============================================================================
binaryStringToIntList :: String -> [Int]
binaryStringToIntList ('0':rest) = 0:(binaryStringToIntList rest)
binaryStringToIntList ('1':rest) = 1:(binaryStringToIntList rest)
binaryStringToIntList [] = []
-- =============================================================================
binaryIntListToDecimal :: [Int] -> Int
binaryIntListToDecimal binaryIntList = binaryIntListToDecimal_ binaryIntList 0 (2^((length binaryIntList) - 1))
binaryIntListToDecimal_ :: [Int] -> Int -> Int -> Int
binaryIntListToDecimal_ [] acc mul = acc
binaryIntListToDecimal_ (0:xs) acc mul = binaryIntListToDecimal_ xs acc (div mul 2)
binaryIntListToDecimal_ (1:xs) acc mul = binaryIntListToDecimal_ xs (acc+mul) (div mul 2)
-- =============================================================================
countOnes :: [Int] -> [Int] -> [Int]
countOnes [] [] = []
countOnes (1:bs) (cnt:restcnt) = (cnt+1):(countOnes bs restcnt)
countOnes (_:bs) (cnt:restcnt) =  cnt   :(countOnes bs restcnt)
-- =============================================================================
countZeros :: [Int] -> [Int] -> [Int]
countZeros [] [] = []
countZeros (0:bs) (cnt:restcnt) = (cnt+1):(countZeros bs restcnt)
countZeros (_:bs) (cnt:restcnt) =  cnt   :(countZeros bs restcnt)
-- =============================================================================
countBitsList :: [[Int]] -> ([Int],[Int])
countBitsList bits =
  countBitsList_ bits (zerosCounter,onesCounter)
  where n            = length (head bits)
        zerosCounter = replicate n 0
        onesCounter  = replicate n 0
-- =============================================================================
countBitsList_ :: [[Int]] -> ([Int],[Int]) -> ([Int],[Int])
countBitsList_ [] counters = counters
countBitsList_ (bits:restbits) (zerosCounter, onesCounter) =
  countBitsList_ restbits (updatedZeros, updatedOnes)
  where updatedZeros = countZeros bits zerosCounter
        updatedOnes  = countOnes bits onesCounter
-- =============================================================================
binaryGamma :: [Int] -> [Int] -> [Int]
binaryGamma [] [] = []
binaryGamma (z:zs) (o:os)
  | z > o     = 0 : binaryGamma zs os
  | otherwise = 1 : binaryGamma zs os
-- =============================================================================
gamma :: [Int] -> [Int] -> Int
gamma zs os = binaryIntListToDecimal (binaryGamma zs os)
-- =============================================================================
binaryEpsilonRate :: [Int] -> [Int] -> [Int]
binaryEpsilonRate [] [] = []
binaryEpsilonRate (z:zs) (o:os)
  | z < o     = 0 : binaryEpsilonRate zs os
  | otherwise = 1 : binaryEpsilonRate zs os
-- =============================================================================
epsilonRate :: [Int] -> [Int] -> Int
epsilonRate zs os = binaryIntListToDecimal (binaryEpsilonRate zs os)
-- =============================================================================
test1 :: [String] -> IO ()
test1 input = do
  let ints                       = map binaryStringToIntList input
  let (zerosCounter,onesCounter) = countBitsList ints
  let g = gamma       zerosCounter onesCounter
  let e = epsilonRate zerosCounter onesCounter
  print (g==22)
  print (e==9)
  print (g*e==198)
  return ()
-- =============================================================================
part1 :: [String] -> IO ()
part1 input = do
  let ints                       = map binaryStringToIntList input
  let (zerosCounter,onesCounter) = countBitsList ints
  let g = gamma       zerosCounter onesCounter
  let e = epsilonRate zerosCounter onesCounter
  print g
  print e
  print (g*e)
  return ()

-- =============================================================================
part2 :: [String] -> IO ()
part2 input = do
  return ()

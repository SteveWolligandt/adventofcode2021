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
count :: [[Int]] -> ([Int],[Int])
count bits =
  countBitsList_ bits (zerosCounter,onesCounter)
  where n            = length (head bits)
        zerosCounter = replicate n 0
        onesCounter  = replicate n 0
-- =============================================================================
countHeads :: [([Int],[Int])] -> Int -> Int -> (Int,Int)
countHeads [] numZeros numOnes 
  = (numZeros, numOnes)
countHeads (((0:_),_):rest) numZeros numOnes 
  = countHeads rest (numZeros+1) numOnes
countHeads (((1:_),_):rest) numZeros numOnes 
  = countHeads rest numZeros (numOnes+1)
-- =============================================================================
countBitsList_ :: [[Int]] -> ([Int],[Int]) -> ([Int],[Int])
countBitsList_ [] counters = counters
countBitsList_ (bits:restbits) (zerosCounter, onesCounter) =
  countBitsList_ restbits (updatedZeros, updatedOnes)
  where updatedZeros = countZeros bits zerosCounter
        updatedOnes  = countOnes bits onesCounter
-- =============================================================================
binaryGamma :: ([Int], [Int]) -> [Int]
binaryGamma ([],[]) = []
binaryGamma ((numZeros:zs),(numOnes:os))
  | numZeros > numOnes     = 0 : binaryGamma (zs,os)
  | otherwise = 1 : binaryGamma (zs,os)
-- =============================================================================
gamma :: ([Int],[Int]) -> Int
gamma counts = binaryIntListToDecimal (binaryGamma counts)
-- =============================================================================
binaryEpsilonRate :: ([Int], [Int]) -> [Int]
binaryEpsilonRate ([],[]) = []
binaryEpsilonRate ((numZeros:zs),(numOnes:os))
  | numZeros < numOnes     = 0 : binaryEpsilonRate (zs,os)
  | otherwise = 1 : binaryEpsilonRate (zs,os)
-- =============================================================================
epsilonRate :: ([Int],[Int]) -> Int
epsilonRate counts = binaryIntListToDecimal (binaryEpsilonRate counts)
-- =============================================================================
binaryOxygen :: [([Int],[Int])] -> [Int]
binaryOxygen [(_, bin)] = bin
binaryOxygen bins
  | numOnes >= numZeros = binaryOxygen (crop (filterOnes  bins))
  | otherwise           = binaryOxygen (crop (filterZeros bins))
  where filterZeros = filter (\((x:xs),bin) -> x == 0)
        filterOnes  = filter (\((x:xs),bin) -> x == 1)
        crop        = map (\(cropped,bins) -> (tail cropped, bins))
        (numZeros,numOnes)       = countHeads bins 0 0
-- =============================================================================
binaryCO2 :: [([Int],[Int])] -> [Int]
binaryCO2 [(_, bin)] = bin
binaryCO2 bins
  | numOnes < numZeros = binaryCO2 (crop (filterOnes  bins))
  | otherwise          = binaryCO2 (crop (filterZeros bins))
  where filterZeros = filter (\((x:xs),bin) -> x == 0)
        filterOnes  = filter (\((x:xs),bin) -> x == 1)
        crop        = map (\(cropped,bins) -> (tail cropped, bins))
        (numZeros,numOnes)       = countHeads bins 0 0
-- =============================================================================
oxygen :: [[Int]] -> Int
oxygen bins = binaryIntListToDecimal (binaryOxygen (zip bins bins))
-- =============================================================================
co2 :: [[Int]] -> Int
co2 bins  = binaryIntListToDecimal (binaryCO2 (zip bins bins))
-- =============================================================================
test1 :: [String] -> IO ()
test1 input = do
  let bins   = map binaryStringToIntList input
  let counts = count bins
  let g = gamma       counts
  let e = epsilonRate counts
  let o = oxygen bins
  let c = co2 bins
  print (g==22)
  print (e==9)
  print (g*e==198)
  print (o==23)
  print (c==10)
  print o
  print c
-- =============================================================================
part1 :: [String] -> IO ()
part1 input = do
  let bins                       = map binaryStringToIntList input
  let counts = count bins
  let g = gamma       counts
  let e = epsilonRate counts
  let o = oxygen bins
  let c = co2 bins
  putStrLn "gamma:"
  print g
  putStrLn "epsilonRate:"
  print e
  putStrLn "gamma*epsilonRate:"
  print (g*e)
  putStrLn "oxygen:"
  print (o)
  putStrLn "co2:"
  print (c)
  putStrLn "oxygen*co2:"
  print (o*c)

-- =============================================================================
part2 :: [String] -> IO ()
part2 input = do
  return ()

module AdventOfCode.Day4 where
import Data.List.Split
-- =============================================================================
parseNumbers :: String -> [Int]
parseNumbers numsString = map read (splitOn "," numsString)
-- =============================================================================
parseBingoFields :: [String] -> [[[Int]]]
parseBingoFields [] = []
parseBingoFields (_:l1:l2:l3:l4:l5:rest) =
  (map (map read) (map words [l1,l2,l3,l4,l5])):(parseBingoFields rest)
-- =============================================================================
parse :: [String] -> ([Int], [[[Int]]])
parse (nums:bingoData) = (parseNumbers nums, parseBingoFields bingoData)
-- =============================================================================
fieldIsFinished :: [[Bool]] -> Bool
fieldIsFinished [[True,True,True,True,True],_,_,_,_] = True
fieldIsFinished [_,[True,True,True,True,True],_,_,_] = True
fieldIsFinished [_,_,[True,True,True,True,True],_,_] = True
fieldIsFinished [_,_,_,[True,True,True,True,True],_] = True
fieldIsFinished [_,_,_,_,[True,True,True,True,True]] = True
fieldIsFinished [[True,_,_,_,_],[True,_,_,_,_],[True,_,_,_,_],[True,_,_,_,_],[True,_,_,_,_]] = True
fieldIsFinished [[_,True,_,_,_],[_,True,_,_,_],[_,True,_,_,_],[_,True,_,_,_],[_,True,_,_,_]] = True
fieldIsFinished [[_,_,True,_,_],[_,_,True,_,_],[_,_,True,_,_],[_,_,True,_,_],[_,_,True,_,_]] = True
fieldIsFinished [[_,_,_,True,_],[_,_,_,True,_],[_,_,_,True,_],[_,_,_,True,_],[_,_,_,True,_]] = True
fieldIsFinished [[_,_,_,_,True],[_,_,_,_,True],[_,_,_,_,True],[_,_,_,_,True],[_,_,_,_,True]] = True
fieldIsFinished _ = False
-- =============================================================================
fieldsAreFinished :: [[[Bool]]] -> [Bool]
fieldsAreFinished [] = []
fieldsAreFinished (state:states) = fieldIsFinished state:fieldsAreFinished states
-- =============================================================================
-- Checks if the current bingo number is in a line and updates state
updateStateLine :: Int -> [Int] -> [Bool] -> [Bool]
updateStateLine number [x1,x2,x3,x4,x5] [s1,s2,s3,s4,s5] 
  | number == x1 = [True,s2,s3,s4,s5]
  | number == x2 = [s1,True,s3,s4,s5]
  | number == x3 = [s1,s2,True,s4,s5]
  | number == x4 = [s1,s2,s3,True,s5]
  | number == x5 = [s1,s2,s3,s4,True]
  | otherwise    = [s1,s2,s3,s4,s5]
-- =============================================================================
-- Checks if the current bingo number is in a whole field and updates state
updateState :: Int -> [[Int]] -> [[Bool]] -> [[Bool]]
updateState number [] [] = []
updateState number (line:restlines) (stateline:reststatelines) =
  (updateStateLine number line stateline):(updateState number restlines reststatelines)
-- =============================================================================
updateStates :: Int -> [[[Int]]] -> [[[Bool]]] -> [[[Bool]]]
updateStates _ [] [] = []
updateStates n (field:fields) (state:states) =
  (updateState n field state):(updateStates n fields states)
-- =============================================================================
initialFieldState :: [[Bool]]
initialFieldState = take 5 (repeat (take 5 (repeat False)))
-- =============================================================================
play :: [Int] -> [[[Int]]] -> Int
play numbers fields =
  play_ numbers fields (take (length fields) (repeat initialFieldState))
-- =============================================================================
play_ :: [Int] -> [[[Int]]] -> [[[Bool]]] -> Int
play_ [] _ _ = 0
play_ (n:ns) fields states
  | elem True finishedFields = part1Result n fields updatedStates finishedFields
  | otherwise                = play_ ns fields updatedStates
  where updatedStates  = updateStates n fields states
        finishedFields = fieldsAreFinished updatedStates
-- =============================================================================
part1Result :: Int -> [[[Int]]] -> [[[Bool]]] -> [Bool] -> Int
part1Result_ :: Int -> [[Int]] -> [[Bool]] -> Int
part1Result number [] [] [] = 0
part1Result number (field:fields) (state:states) (True:finished) = part1Result_ number field state
part1Result number (field:fields) (state:states) (False:finished) = part1Result number fields states finished
part1Result_ number field state = (sumOfUnmarkedField field state) * number
-- =============================================================================
sumOfUnmarkedLine :: [Int] -> [Bool] -> Int
sumOfUnmarkedLine [] [] = 0
sumOfUnmarkedLine (x:xs) (True:ss) = sumOfUnmarkedLine xs ss
sumOfUnmarkedLine (x:xs) (False:ss) = x + (sumOfUnmarkedLine xs ss)
-- =============================================================================
sumOfUnmarkedField :: [[Int]] -> [[Bool]] -> Int
sumOfUnmarkedField [] [] = 0
sumOfUnmarkedField (l:ls) (s:ss) = (sumOfUnmarkedLine l s)+(sumOfUnmarkedField ls ss)
-- =============================================================================
test1 :: [String] -> IO ()
test1 input = do
  putStrLn "=========== TEST 1"
  let (numbers, fields) = parse input
  let sumOfUnmarkedFieldResult = sumOfUnmarkedField [[14, 21, 17, 24, 4],[10, 16, 15,  9, 19],[18,  8, 23, 26, 20],[22, 11, 13,  6,  5],[2,  0, 12,  3,  7]] [[True,True,True,True,True],[False,False,False,True,False],[False,False,True,False,False],[False,True,False,False,True],[True,True,False,False,True]]
  print(188 == sumOfUnmarkedFieldResult)
  let playResult = play numbers fields
  print playResult 
  print (4512 == playResult)
  putStrLn "=========== END TEST 1"
-- =============================================================================
part1 :: [String] -> IO ()
part1 input = do
  putStrLn "=========== PART 1"
  let (numbers, fields) = parse input
  let part1Result = play numbers fields
  print part1Result
  putStrLn "=========== END PART 1"
-- =============================================================================
part2 :: [String] -> IO ()
part2 input = do
  putStrLn "part2"

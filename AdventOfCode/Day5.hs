module AdventOfCode.Day5 where
import Data.List.Split

type IntPair = (Int,Int)
type Pos     = IntPair
type PosPair = (Pos,Pos)

readInt :: String -> Int
readInt s = read s

boundingbox :: [PosPair] -> IntPair
boundingbox ps = boundingbox_ ps (0,0)

boundingbox_ :: [PosPair] -> IntPair -> IntPair
boundingbox_ [] bb = bb
boundingbox_ (((x0,y0),(x1,y1)):pairs) (xbb,ybb) =
  boundingbox_ pairs (max x0 (max x1 xbb), max y0 (max y1 ybb))

parse :: [String] -> [PosPair]
parse [] = []
parse (line:lines) =
  ((readInt x0str,readInt y0str),(readInt x1str,readInt y1str)):parse lines
  where [p0str,p1str]  = splitOn "->" line
        [x0str, y0str] = splitOn "," p0str
        [x1str, y1str] = splitOn "," p1str

constructField :: IntPair -> [[Int]]
constructField (x,y) =  take y (repeat (take x (repeat 0)))

--fillField :: [[Int]] -> [PosPair] -> [[Int]]
--fillField field [] = field
--fillField field (line:lines) = fillField (fieldField_ field line) lines
--
--fillField_ :: [[Int]] -> PosPair -> [[Int]]
--fillField_ field ((x0,y0),(x1,y1))
--  | x0 == x1 =
--  | y0 == y1 =
--  | otherwise = field

filterNonDiagnoal :: [[Int]]  -> [[Int]]
filterNonDiagnoal pairs = filter (/((x0,y0),(x1,y1)) -> x0 == x1 || y0 == y1) pairs

test1 :: [String] -> IO ()
test1 input = do
  print field
  return ()
  where posPairs = parse input
        field    = constructField (boundingbox posPairs)

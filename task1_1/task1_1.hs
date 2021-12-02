import System.IO

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

main = do
  handle <- openFile "input.txt" ReadMode  
  content <- hGetContents handle
  w <- wordsWhen (=='\n') content
  putStr w[1]
  hClose handle  

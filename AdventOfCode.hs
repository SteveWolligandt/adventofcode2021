module AdventOfCode (operateOnInput) where
-- =============================================================================
import System.IO
-- =============================================================================
operateOnInput :: FilePath -> ([String] -> IO a) -> IO ()
operateOnInput filepath operator = do
  inputhandle <- openFile filepath ReadMode
  input       <- hGetContents inputhandle
  operator (lines input)
  hClose inputhandle

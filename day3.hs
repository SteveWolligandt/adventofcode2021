module Main where
import AdventOfCode
import AdventOfCode.Day3
--import AdventOfCode.Day3.Part1
--import AdventOfCode.Day3.Part2

main = do
  operateOnInput "test.day3.txt" test1
  putStrLn ""
  operateOnInput "input.day3.txt" part1
  putStrLn ""
  operateOnInput "input.day3.txt" part2

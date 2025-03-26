module Days.Day01 (solution) where

import           AOC
import           Control.Arrow

type Input = String

part1 :: Input -> Int
part1 _ = 0

part2 :: Input -> Int
part2 _ = 0

getInput :: FilePath -> IO Input
getInput input = todo

solution :: String -> IO (Int, Int)
solution day = getInput ("input/" <> day <> ".input") >>= pure . (part1 &&& part2)

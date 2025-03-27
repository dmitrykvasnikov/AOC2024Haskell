module Days.Day02 (solution) where

import           Control.Arrow   ((&&&))
import           Data.List.Split (splitOn)

type Level = [Int]

type Input = [Level]

getInput :: FilePath -> IO Input
getInput fp = do
  rawInput <- readFile fp
  pure $ map level . lines $ rawInput
  where
    level :: String -> [Int]
    level = map read . splitOn " "

isSafeLevel :: Level -> Bool
isSafeLevel l@(a : b : _) = go (a - b) l
  where
    go :: Int -> [Int] -> Bool
    go k (x : y : rest) = let d = x - y in k * d > 0 && abs d >= 1 && abs d <= 3 && go k (y : rest)
    go _ _ = True
isSafeLevel _ = True

dampener :: Level -> [Level]
dampener []         = []
dampener (x : rest) = rest : map (x :) (dampener rest)

part1 :: Input -> Int
part1 = length . filter isSafeLevel

part2 :: Input -> Int
part2 = length . filter (any isSafeLevel) . map dampener

solution :: String -> IO (Int, Int)
solution day = getInput ("input/" <> day <> ".input") >>= pure . (part1 &&& part2)

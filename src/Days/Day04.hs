module Days.Day04 (solution) where

import           Control.Arrow ((&&&))
import           Control.Monad (guard)
import           Data.List     (isPrefixOf, sort, tails, transpose)

type Input = [String]

rotations :: [[a]] -> [[[a]]]
rotations = take 4 . iterate (transpose . reverse)

diagonals :: [[a]] -> [[a]]
diagonals []           = []
diagonals i@(_ : rest) = transpose (zipWith drop [0 ..] i) <> diagonals rest

possibleLines :: [[a]] -> [[a]]
possibleLines i = rotations i >>= \g -> (g >>= tails) <> diagonals g

subGrids :: Int -> [[a]] -> [[[a]]]
subGrids s g = do
  grid <- tails g
  let slice = take s grid
  guard $ length slice == s
  transpose . map (map (take s) . tails) $ slice

isXMAS :: [String] -> Bool
isXMAS [[a, _, b], [_, 'A', _], [d, _, e]] = sort [a, e] == "MS" && sort [b, d] == "MS"
isXMAS _ = False

part1 :: Input -> Int
part1 = length . filter (isPrefixOf "XMAS") . possibleLines

part2 :: Input -> Int
part2 = length . filter isXMAS . subGrids 3

getInput :: FilePath -> IO Input
getInput fp = lines <$> readFile fp

solution :: String -> IO (Int, Int)
solution day = getInput ("input/" <> day <> ".input") >>= pure . (part1 &&& part2)

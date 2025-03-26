module Days.Day01 (solution) where

import           Control.Arrow    ((&&&))
import           Data.Char        (isDigit, isSpace)
import           Data.Composition ((.:))
import           Data.List        (sort)
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as M (empty, insertWith, lookup)

type Input = ([Int], [Int])

dict :: [Int] -> Map Int Int
dict = foldl' (\m e -> M.insertWith (+) e 1 m) M.empty

part1 :: Input -> Int
part1 (l, r) = sum . zipWith (abs .: (-)) (sort l) $ sort r

part2 :: Input -> Int
part2 (l, r) = sum . map (maybe 0 . (*) <*> flip M.lookup (dict r)) $ l

getInput :: FilePath -> IO Input
getInput fp = (lines <$> readFile fp) >>= (pure . unzip . map parse)
  where
    parse :: String -> (Int, Int)
    parse = (read . takeWhile isDigit &&& read . takeWhile isDigit . dropWhile isSpace . dropWhile isDigit)

solution :: String -> IO (Int, Int)
solution day = getInput ("input/" <> day <> ".input") >>= pure . (part1 &&& part2)

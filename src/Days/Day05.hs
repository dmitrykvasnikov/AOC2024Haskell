module Days.Day05 where

import           AOC
import           Control.Arrow          ((&&&))
import           Data.Char              (isDigit)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M (empty, insertWith, lookup)
import           Data.Maybe             (fromJust)
import           Data.Sort              (sortBy)
import           Text.Regex.Applicative (RE, many, match, psym, some, sym)

type Page = Int

type Rule = (Page, Page)

type Update = [Page]

type Parser = RE Char

type Input = ([Rule], [Update])

type Dict = Map Page [Page]

decimal :: Parser Int
decimal = read <$> some (psym isDigit)

rule :: Parser Rule
rule = (,) <$> (decimal <* sym '|') <*> (decimal)

update :: Parser Update
update = (:) <$> decimal <*> many (sym ',' *> decimal)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p)

input :: Parser Input
input = (,) <$> (sepBy rule (sym '\n') <* many (sym '\n')) <*> (sepBy update (sym '\n') <* many (sym '\n'))

dict :: [Rule] -> Dict
dict = foldl' (\d (pb, pa) -> M.insertWith (<>) pb [pa] d) M.empty

comparePages :: Dict -> Page -> Page -> Ordering
comparePages d p1 p2 = if elem p2 (fromJust . M.lookup p1 $ d) then GT else LT

isValidUpdate :: Dict -> Update -> Bool
isValidUpdate _ [] = True
isValidUpdate _ [u] = True
isValidUpdate d (p : ps) = checkPage && isValidUpdate d ps
  where
    pagesAfter = fromJust $ M.lookup p d
    checkPage = all (flip elem pagesAfter) $ ps

middlePage :: Update -> Page
middlePage u = let l = length u in head . drop (l `div` 2) $ u

part1 :: Input -> Int
part1 (rules, updates) = sum . map middlePage . filter (isValidUpdate d) $ updates
  where
    d = dict rules

part2 :: Input -> Int
part2 (rules, updates) = sum . map middlePage . map (sortBy (comparePages d)) . filter (not . isValidUpdate d) $ updates
  where
    d = dict rules

getInput :: FilePath -> IO Input
getInput fp = readFile fp >>= pure . fromJust . match input

solution :: String -> IO (Int, Int)
solution day = getInput ("input/" <> day <> ".input") >>= pure . (part1 &&& part2)

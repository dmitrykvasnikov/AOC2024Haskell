module Days.Day06 (solution) where

import           AOC
import           Control.Arrow ((&&&))
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Debug.Trace

data Entity = Start | Empty | Obstacle deriving (Eq, Show)

data Guard = Guard { pos :: Point
                   , dir :: Direction
                   }
  deriving (Eq, Ord, Show)

type Input = Grid Entity

charToEntity :: Char -> Entity
charToEntity '#' = Obstacle
charToEntity '^' = Start
charToEntity _   = Empty

startGuard :: Grid Entity -> Guard
startGuard g = case gridLookup g Start of
  Just p  -> Guard p N
  Nothing -> error "No GUARD presented in input"

-- we don't account case when guard is surrounded by obstacles from all 8 directions,
-- in this case this function will be in indless loop
moveGuard :: Grid Entity -> Guard -> Guard
moveGuard grid (Guard p d) =
  let newP = move p d
   in case grid !? newP of
        Just Obstacle -> moveGuard grid (Guard p $ turnCW . turnCW $ d)
        _             -> Guard newP d

isLoop :: Grid Entity -> Guard -> Bool
isLoop grid guard = go guard S.empty
  where
    go :: Guard -> Set Guard -> Bool
    go guard' visited
      | S.member guard' visited = True
      | inBounds grid (pos newGuard) = go newGuard (S.insert newGuard visited)
      | otherwise = False
      where
        newGuard = moveGuard grid guard'

part1 :: Input -> Int
part1 grid =
  let guard = startGuard grid
   in S.size . S.fromList . map pos . takeWhile (\(Guard p _) -> inBounds grid p) . iterate (moveGuard grid) $ guard

part2 :: Input -> Int
part2 grid =
  let guard = startGuard grid
   in length . filter id . map (\p -> flip isLoop guard . updateGrid grid p $ Obstacle) . getAllPoints $ grid

getInput :: FilePath -> IO Input
getInput fp = do
  rawInput <- readFile fp
  pure . gridFrom2DList . map (map charToEntity) . lines $ rawInput

solution :: String -> IO (Int, Int)
solution day = getInput ("input/" <> day <> ".input") >>= pure . (part1 &&& part2)

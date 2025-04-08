module Days.Day06 (solution) where

import           AOC
import           Control.Arrow              ((&&&))
import           Control.Monad.State.Strict
import           Data.Set                   (Set)
import qualified Data.Set                   as S

data Entity = Start | Empty | Obstacle deriving (Eq, Show)

data Guard = Guard { pos :: Point
                   , dir :: Direction
                   }
  deriving (Eq, Ord, Show)

data Result = Leave | Loop deriving (Eq)

type Input = Grid Entity

charToEntity :: Char -> Entity
charToEntity '#' = Obstacle
charToEntity '^' = Start
charToEntity _   = Empty

startGuard :: Grid Entity -> Guard
startGuard g = case gridLookup g Start of
  Just p  -> Guard p N
  Nothing -> error "No GUARD presented in input"

runGuard :: Grid Entity -> Guard -> (Result, Set Guard)
runGuard grid guard = runState (go guard) S.empty
  where
    go :: Guard -> State (Set Guard) Result
    go guard = do
      let movedGuard = moveGuard grid guard
      case inBounds grid (pos movedGuard) of
        False -> pure Leave
        True ->
          gets (S.member movedGuard) >>= \case
            True  -> pure Loop
            False -> modify (S.insert movedGuard) >> go movedGuard

-- we don't account case when guard is surrounded by obstacles from all 8 directions,
-- in this case this function will be in indless loop
moveGuard :: Grid Entity -> Guard -> Guard
moveGuard grid (Guard p d) =
  let newP = move p d
   in case grid !? newP of
        Just Obstacle -> moveGuard grid (Guard p $ turnCW . turnCW $ d)
        _             -> Guard newP d

part1 :: Input -> Int
part1 grid =
  let guard = startGuard grid
   in S.size . S.fromList . map pos . S.toList . snd . runGuard grid $ guard

part2 :: Input -> Int
part2 grid =
  let guard = startGuard grid
      points = getPointsWith grid (/= Obstacle)
   in length . filter ((== Loop) . fst . flip runGuard guard . (\p -> updateGrid grid p Obstacle)) $ points

getInput :: FilePath -> IO Input
getInput fp = do
  rawInput <- readFile fp
  pure . gridFrom2DList . map (map charToEntity) . lines $ rawInput

solution :: String -> IO (Int, Int)
solution day = getInput ("input/" <> day <> ".input") >>= pure . (part1 &&& part2)

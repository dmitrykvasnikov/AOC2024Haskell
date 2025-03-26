module AOC.Point where

import           Control.Lens ((^.))
import           Linear.V2    (V2 (..), _x, _y)

-- Coordinate represented as Vector to get all numeric operation out of the box
type Point = V2 Int

-- Vector is implemented in the same way as Coordinate for easier calculations
type Vector = Point

point :: Int -> Int -> Point
point = V2

pX, pY :: Point -> Int
pX p = p ^. _x
pY p = p ^. _y

data Direction = N | NE | E | SE | S | SW | W | NW deriving
  ( Bounded
  , Enum
  , Eq
  , Ord
  , Show
  )

directionVector :: Direction -> Vector
directionVector = \case
  N  -> point 0 n
  NE -> point e n
  E  -> point e 0
  SE -> point e s
  S  -> point 0 s
  SW -> point w s
  W  -> point w 0
  NW -> point w n
  where
    n = -1
    s = 1
    w = -1
    e = 1

-- helpers for clockwise and counter-clockwise turn
turnCW, turnCCW :: Direction -> Direction
turnCW dir = if dir == NW then N else succ dir
turnCCW dir = if dir == N then NW else pred dir

-- helpers for 4 and 8 neghbours of given point
get4Points, get8Points :: Point -> [Point]
get4Points p = map ((+ p) . directionVector) [N, E, S, W]
get8Points p = map ((+ p) . directionVector) [N, NE, E, SE, S, SW, W, NW]

module AOC.Grid where

import           AOC.Point
import           Data.Vector (Vector)
import qualified Data.Vector as V (fromList, toList, (!))

type Height = Int

type Width = Int

type Index = Int

data Grid a = Grid { gData   :: Vector a
                   , gWidth  :: Width
                   , gHeight :: Height
                   }
  deriving (Show)

gridFromList :: Width -> Height -> [a] -> Grid a
gridFromList w h raw
  | w * h /= length raw' = error "Incorrect width and length"
  | otherwise = Grid (V.fromList raw') w h
  where
    !raw' = take (w * h) raw

gridFrom2DList :: [[a]] -> Grid a
gridFrom2DList raw = gridFromList (length . head $ raw) (length raw) (concat raw)

gridToList :: Grid a -> [a]
gridToList = V.toList . gData

toIndex :: Grid a -> Point -> Index
toIndex g p = pY p * gWidth g + pX p

fromIndex :: Grid a -> Index -> Point
fromIndex g i = let (y, x) = divMod i (gWidth g) in point x y

xRange, yRange :: Grid a -> [Int]
xRange g = take (gWidth g) [0 ..]
yRange g = take (gHeight g) [0 ..]

inBounds :: Grid a -> Point -> Bool
inBounds g p =
  let x = pX p
      y = pY p
   in and [x >= 0, y >= 0, x < gWidth g, y < gHeight g]

getAllPoints :: Grid a -> [Point]
getAllPoints g = point <$> xRange g <*> yRange g

get4GridPoints, get8GridPoints :: Grid a -> Point -> [Point]
get4GridPoints g = filter (inBounds g) . get4Points
get8GridPoints g = filter (inBounds g) . get8Points

get4GridValues, get8GridValues :: Grid a -> Point -> [a]
get4GridValues g = map (g !) . filter (inBounds g) . get4Points
get8GridValues g = map (g !) . filter (inBounds g) . get8Points

(!) :: Grid a -> Point -> a
g ! p = gData g V.! toIndex g p

(!?) :: Grid a -> Point -> Maybe a
(!?) g p
  | inBounds g p = Just $ g ! p
  | otherwise = Nothing

displayGrid :: (a -> String) -> Grid a -> IO ()
displayGrid f g = mapM_ putStrLn [concat [f (g ! point x y) | x <- xRange g] | y <- yRange g]

ray :: Grid a -> Direction -> Point -> [Point]
ray g d p = takeWhile (inBounds g) . iterate (flip move d) $ p

rayValues :: Grid a -> Direction -> Point -> [a]
rayValues g d = map (g !) . ray g d

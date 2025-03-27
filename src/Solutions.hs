module Solutions (solutions) where

import           Days.Day01
import           Days.Day02

solutions :: [(String, String -> IO (Int, Int))]
solutions =
  [ ("1", Days.Day01.solution),
    ("2", Days.Day02.solution)
  ]

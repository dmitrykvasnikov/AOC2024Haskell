module Solutions (solutions) where

import           Days.Day01
import           Days.Day02
import           Days.Day03
import           Days.Day04
import           Days.Day05
import           Days.Day06

solutions :: [(String, String -> IO (Int, Int))]
solutions =
  [ ("1", Days.Day01.solution),
    ("2", Days.Day02.solution),
    ("3", Days.Day03.solution),
    ("4", Days.Day04.solution),
    ("5", Days.Day05.solution),
    ("6", Days.Day06.solution)
  ]

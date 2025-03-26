module Solutions (solutions) where

import           Days.Day01

solutions :: [(String, String -> IO (Int, Int))]
solutions = [("1", Days.Day01.solution)]

module Main where

import           Solutions
import           System.IO

main :: IO ()
main = do
  putStrLn "Welcome to the AOC 2024 in HASKELL!!!\n"
  printSolution

printSolution :: IO ()
printSolution = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStr "Enter day number [1-24] (ENTER to exit): "
  getLine >>= \case
    "" -> putStrLn "Bye-bye!!!"
    day -> case lookup day solutions of
      Nothing  -> putStrLn "Incorrect day number" >> printSolution
      Just sol -> (sol day) >>= (putStrLn . show) >> printSolution

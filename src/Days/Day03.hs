module Days.Day03 (solution) where

import           Control.Applicative    (asum)
import           Control.Arrow          ((&&&))
import           Control.Monad          (replicateM)
import           Data.Char              (isDigit)
import           Text.Regex.Applicative (RE, findFirstInfix, psym, string, sym,
                                         (<|>))

data Instruction = Do
                 | Dont
                 | Mul Int Int
  deriving (Show)

type Input = [Instruction]

type Parser = RE Char

instruction :: Parser Instruction
instruction = (doP <|> dontP <|> mulP)
  where
    doP = Do <$ string "do()"
    dontP = Dont <$ string "don't()"
    mulP = Mul <$ string "mul(" <*> (decimal <* sym ',') <*> (decimal <* sym ')')
    decimal = read <$> asum [replicateM n (psym isDigit) | n <- [1 .. 3]]

calculate :: Instruction -> Int
calculate (Mul x y) = x * y
calculate _         = 0

part1 :: Input -> Int
part1 = sum . map calculate

part2 :: Input -> Int
part2 = sum . map calculate . go True
  where
    go :: Bool -> [Instruction] -> [Instruction]
    go _ []            = []
    go _ (Do : rest)   = go True rest
    go _ (Dont : rest) = go False rest
    go f (mul : rest)  = if f then mul : go f rest else go f rest

getInput :: FilePath -> IO Input
getInput fp = do
  rawInput <- readFile fp
  pure $ go rawInput
  where
    go :: String -> [Instruction]
    go "" = []
    go i = case findFirstInfix instruction i of
      Nothing               -> []
      (Just (_, ins, rest)) -> ins : go rest

solution :: String -> IO (Int, Int)
solution day = getInput ("input/" <> day <> ".input") >>= pure . (part1 &&& part2)

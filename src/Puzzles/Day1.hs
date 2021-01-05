module Puzzles.Day1 ( part1, part2 ) where

import           Data.Set

import           Puzzles.Input1

sortedInputs :: Set Integer
sortedInputs = fromList input

part1 :: Integer
part1 = go input
  where
    go (x : xs)
        | 2020 - x `elem` sortedInputs = x * (2020 - x)
        | otherwise = go xs

part2 :: Integer
part2 = go mempty input
  where
    go tried (x : xs)
        | 2020 - x `elem` tried = go tried xs
        | otherwise =
            maybe (go (insert (2020 - x) tried) xs) (x *) (solveFor (2020 - x))

solveFor :: Integer -> Maybe Integer
solveFor n = go input
  where
    go [] = Nothing
    go (x : xs)
        | n - x `elem` sortedInputs = Just $ x * (n - x)
        | otherwise = go xs

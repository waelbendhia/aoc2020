module Puzzles.Day3 ( part1, part2 ) where

import           Puzzles.Input3
import qualified Data.Text      as T

part1 :: Integer
part1 = treesPerSlope 3 1

treesPerSlope :: Int -> Int -> Integer
treesPerSlope sx sy = go 0 input
  where
    l = T.length $ head input
    go _ [] = 0
    go x ts@(r : rs) = go ((x + sx) `mod` l) (drop sy ts)
        + if T.index r x == '#' then 1 else 0

part2 :: Integer
part2 = product $ uncurry treesPerSlope
    <$> [ ( 1, 1 ), ( 3, 1 ), ( 5, 1 ), ( 7, 1 ), ( 1, 2 ) ]

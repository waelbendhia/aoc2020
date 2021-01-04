module Puzzles.Day15 ( part1, part2 ) where

import qualified Data.IntMap as M

testInput :: [ Int ]
testInput = [ 0, 3, 6 ]

input :: [ Int ]
input = [ 9, 12, 1, 4, 17, 0, 18 ]

solvePart1 i max = go (M.fromList (zip i $ zip [ 1 .. ] [ 1 .. ])) (last i)
    (length i + 1)
  where
    go mem prev i
        = let cur
                  | Just ( ft, lt ) <- M.lookup prev mem, ft /= lt = lt - ft
                  | otherwise = 0
              mem' = M.insertWith (\_ ( _, lt ) -> ( lt, i )) cur ( i, i ) mem
        in if i == max then cur else go mem' cur (i + 1)

part1 = solvePart1 input 2020

part2 = solvePart1 input 30000000

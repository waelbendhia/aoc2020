module Puzzles.Day9 ( part1, part2 ) where

import qualified Data.IntSet    as S
import qualified Data.Vector    as V

import           Puzzles.Input9

data S = S { preamble :: S.IntSet, position :: Int, l :: V.Vector Int }
    deriving ( Show, Eq )

part1 = walkList (S (S.fromList $ take 25 input) 25 (V.fromList input))

walkList (S p pos l)
    | pos >= V.length l = error "all valid"
    | currentValid =
        walkList (S (S.delete (l V.! pos - 25) $ S.insert current p)
                    (pos + 1)
                    l)
    | otherwise = current
  where
    current = l V.! pos
    currentValid = any (\x -> S.member (current - x) p) $ S.toList p

findContiguousSet t v = go 0 1 (v V.! 0 + v V.! 1)
  where
    go start end total
        | total > t =
            go (start + 1) (start + 2) (v V.! (start + 1) + v V.! (start + 2))
        | total == t = (start, end)
        | otherwise = go start (end + 1) (total + v V.! (end + 1))

part2 = V.minimum subset + V.maximum subset
  where
    v = V.fromList input
    (x, y) = findContiguousSet part1 v
    subset = V.drop x $ V.take (y + 1) v

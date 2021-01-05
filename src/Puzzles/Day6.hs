module Puzzles.Day6 ( part1, part2 ) where

import qualified Data.Set       as S
import           Data.Text      as T

import           Puzzles.Input6

sanitizedInput :: [[String]]
sanitizedInput = Prelude.filter (/= "") . fmap unpack . T.splitOn "\n"
    <$> T.splitOn "\n\n" input

part1 :: Int
part1 = sum $ S.size . mconcat . fmap S.fromList <$> sanitizedInput

part2 :: Int
part2 = sum $ S.size . intersectAll . fmap S.fromList <$> sanitizedInput
  where
    intersectAll [x] = x
    intersectAll (x : xs) = x `S.intersection` intersectAll xs
    intersectAll _ = error "well shit"

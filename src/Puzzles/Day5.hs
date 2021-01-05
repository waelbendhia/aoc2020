module Puzzles.Day5 ( part1, part2 ) where

import qualified Data.Set       as S

import           Puzzles.Input5

findSeat :: String -> (Int, Int)
findSeat i = (findRow 0 127 $ take 7 i, findCol 0 7 $ drop 7 i)
  where
    findRow mi mx []
        | mi /= mx = error $ show mi <> " " <> show mx
        | otherwise = mi
    findRow mi mx ('F' : xs) = findRow mi ((mx + mi) `div` 2) xs
    findRow mi mx ('B' : xs) = findRow ((mx + mi + 1) `div` 2) mx xs
    findRow _ _ (c : _) = error [c]
    findCol mi mx []
        | mi /= mx = error $ show mi <> " " <> show mx
        | otherwise = mi
    findCol mi mx ('L' : xs) = findCol mi ((mx + mi) `div` 2) xs
    findCol mi mx ('R' : xs) = findCol ((mx + mi + 1) `div` 2) mx xs
    findCol _ _ (c : _) = error [c]

part1 :: Int
part1 = maximum $ (\(r, c) -> r * 8 + c) . findSeat <$> input

part2 :: Int
part2 = res
  where
    allSeats = do r <- [1 .. 126]
                  c <- [0 .. 7]
                  pure $ r * 8 + c
    seatIDs = S.fromList $ (\(r, c) -> r * 8 + c) . findSeat <$> input
    [res] = filter (\i -> not (i `S.member` seatIDs)
                    && all (`S.member` seatIDs) [i - 1, i + 1])
                   allSeats



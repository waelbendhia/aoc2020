module Puzzles.Day10 ( part1, part2 ) where

import qualified Data.IntMap     as M
import qualified Data.List       as L
import qualified Data.Vector     as V

import           Polysemy
import           Polysemy.State

import           Puzzles.Input10

testInput =
    L.sort [ 28
           , 33
           , 18
           , 42
           , 31
           , 14
           , 46
           , 20
           , 48
           , 47
           , 24
           , 23
           , 49
           , 45
           , 19
           , 38
           , 39
           , 11
           , 1
           , 32
           , 25
           , 35
           , 8
           , 17
           , 7
           , 9
           , 4
           , 2
           , 34
           , 10
           , 3
           ]

getOutputs i = go (0 : L.sort i)
  where
    go [_] = [3]
    go (x : x' : xs) = x' - x : go (x' : xs)

getOnesAndThrees i =
    foldr (\x (ones, threes) -> if x == 1
               then (ones + 1, threes)
               else if x == 3 then (ones, threes + 1) else (ones, threes))
          (0, 0) $ getOutputs i

part1 = ones * threes
  where
    (ones, threes) = getOnesAndThrees input

type IMap = M.IntMap Int

getPossibleConfigurations i = run $ runState @IMap mempty $ go 0
  where
    os = V.fromList $ getOutputs i
    go :: Member (State IMap) r => Int -> Sem r Int
    go i
        | i == (V.length os - 1) = do modify @IMap (M.insert i 1)
                                      pure 1
        | otherwise = do p <- gets @IMap (M.lookup i)
                         maybe (do t <- sum <$> sequence [plus1, plus2, plus3]
                                   t <$ modify @IMap (M.insert i t))
                               pure
                               p
      where
        plus1
            | os V.! i <= 3 = go (i + 1)
            | otherwise = pure 0
        plus2
            | i + 2 >= V.length os = pure 0
            | os V.! i + os V.! (i + 1) <= 3 = go (i + 2)
            | otherwise = pure 0
        plus3
            | i + 3 >= V.length os = pure 0
            | os V.! i + os V.! (i + 1) + os V.! (i + 2) <= 3 = go (i + 3)
            | otherwise = pure 0

part2 :: Int
part2 = snd $ getPossibleConfigurations input

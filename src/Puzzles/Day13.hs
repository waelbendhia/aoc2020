module Puzzles.Day13 ( part1, part2 ) where

import qualified Data.Text       as T
import qualified Data.List       as L
import           Puzzles.Input13
import           Data.Function   ( on )
import           Control.Monad
import           Data.Ord
import           GHC.IO

testInput :: ( T.Text, T.Text )
testInput = ( "939", "7,13,x,x,59,x,31,19" )

fromInput :: ( T.Text, T.Text ) -> ( Integer, [ Integer ] )
fromInput ( d, bs ) = ( read (T.unpack d)
                      , read . T.unpack <$> filter (/= "x") (T.splitOn "," bs)
                      )

parseInput2 :: ( T.Text, T.Text ) -> [ ( Integer, Integer ) ]
parseInput2 ( _, bs ) = f bs' 0
  where
    bs' = T.splitOn "," bs
    f ("x" : rest) i = f rest (i + 1)
    f (n : rest) i = ( read $ T.unpack n, i ) : f rest (i + 1)
    f [] _ = []

solvePart1 ( d, bs ) = s * f s
  where
    s   = L.minimumBy (compare `on` f) bs
    f b = b - (d `mod` b)

part1 :: Integer
part1 = solvePart1 $ fromInput input

solvePart2 i = go bs s s
  where
    (( s, _ ) : bs) = parseInput2 i
    incTs ts period bid boffset
        | (ts + boffset) `mod` bid == 0 = ts
        | otherwise = incTs (ts + period) period bid boffset
    go [] ts _ = ts
    go (( bid, boffset ) : rest) ts period = go rest
        (incTs ts period bid boffset) (lcm period bid)

part2 = solvePart2 input

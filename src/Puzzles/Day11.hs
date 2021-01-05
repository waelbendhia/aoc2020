module Puzzles.Day11 ( part1, part2 ) where

import           Data.Maybe
import qualified Data.Text       as T
import qualified Data.Vector     as V

import           Polysemy
import           Polysemy.Output
import           Polysemy.State

import           Puzzles.Input11

data SeatState = Occupied | Empty | Floor
    deriving ( Show, Eq, Ord )

type SeatPlan = V.Vector (V.Vector SeatState)

fromChar 'L' = Empty
fromChar '#' = Occupied
fromChar '.' = Floor

toChar Empty = 'L'
toChar Occupied = '#'
toChar Floor = '.'

vv !!! (i, j) = (vv V.! i) V.! j

(!!?) :: SeatPlan -> (Int, Int) -> Maybe SeatState
vv !!? (i, j) = (vv V.!? i) >>= (V.!? j)

getSurrounding i j =
    [ (i - 1, j - 1)
    , (i, j - 1)
    , (i + 1, j - 1)
    , (i - 1, j)
    , (i + 1, j)
    , (i - 1, j + 1)
    , (i, j + 1)
    , (i + 1, j + 1)
    ]

stepSeatPlan sp
    | null os = nextState
    | otherwise = stepSeatPlan nextState
  where
    (nextState, (os, ())) = run $ runState sp $ runOutputList @() $ go 0 0
    vLength = V.length sp
    hLength = V.length (sp V.! 0)
    newState :: Int -> Int -> SeatState
    newState i j = let adj = length $ filter (== Occupied) $ catMaybes $
                           (sp !!?) <$> getSurrounding i j
                   in
                       case sp !!! (i, j) of
                           Floor    -> Floor
                           Empty    -> if adj == 0 then Occupied else Empty
                           Occupied -> if adj >= 4 then Empty else Occupied
    go :: (Member (State SeatPlan) r, Member (Output ()) r)
       => Int
       -> Int
       -> Sem r ()
    go i j
        | j == hLength = go (i + 1) 0
        | i == vLength = pure ()
        | otherwise = do
            let s' = newState i j
            if s' /= sp !!! (i, j)
                then do modify (\sp -> sp V.// [(i, (sp V.! i) V.// [(j, s')])])
                        output ()
                else pure ()
            go i (j + 1)

testInput :: [String]
testInput = [ "L.LL.LL.LL"
            , "LLLLLLL.LL"
            , "L.L.L..L.."
            , "LLLL.LL.LL"
            , "L.LL.LL.LL"
            , "L.LLLLL.LL"
            , "..L.L....."
            , "LLLLLLLLLL"
            , "L.LLLLLL.L"
            , "L.LLLLL.LL"
            ]

printSeatPlan :: SeatPlan -> IO ()
printSeatPlan sp = mapM_ print $ V.toList $ fmap toChar . V.toList <$> sp

testInput' = V.fromList $ fmap fromChar . V.fromList <$> testInput

occupiedSeatsAtEnd :: SeatPlan -> Int
occupiedSeatsAtEnd i = length $ filter (== Occupied) $ V.toList
    =<< V.toList (stepSeatPlan i)

part1 = occupiedSeatsAtEnd i
  where
    i = V.fromList $ fmap fromChar . V.fromList <$> input

stepSeatPlan' sp
    | null os = nextState
    | otherwise = stepSeatPlan' nextState
  where
    (nextState, (os, ())) = run $ runState sp $ runOutputList @() $ go 0 0
    vLength = V.length sp
    hLength = V.length (sp V.! 0)
    trav i j di dj = case sp !!? (i, j) of
        Nothing       -> 0
        Just Floor    -> trav (i + di) (j + dj) di dj
        Just Empty    -> 0
        Just Occupied -> 1
    getVisible i j = sum $ (\(di, dj) -> trav (i + di) (j + dj) di dj)
        <$> [ (-1, 0)
            , (-1, 1)
            , (0, 1)
            , (1, 1)
            , (1, 0)
            , (1, -1)
            , (0, -1)
            , (-1, -1)
            ]
    newState :: Int -> Int -> SeatState
    newState i j = let vis = getVisible i j
                   in
                       case sp !!! (i, j) of
                           Floor    -> Floor
                           Empty    -> if vis == 0 then Occupied else Empty
                           Occupied -> if vis >= 5 then Empty else Occupied
    go :: (Member (State SeatPlan) r, Member (Output ()) r)
       => Int
       -> Int
       -> Sem r ()
    go i j
        | j == hLength = go (i + 1) 0
        | i == vLength = pure ()
        | otherwise = do
            let s' = newState i j
            if s' /= sp !!! (i, j)
                then do modify (\sp -> sp V.// [(i, (sp V.! i) V.// [(j, s')])])
                        output ()
                else pure ()
            go i (j + 1)

part2 = length $ filter (== Occupied) $ V.toList =<< V.toList (stepSeatPlan' i)
  where
    i = V.fromList $ fmap fromChar . V.fromList <$> input


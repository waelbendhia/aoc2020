module Puzzles.Day14 where

import qualified Data.Map        as M
import qualified Data.Text       as T
import qualified Data.Vector     as V

import           Polysemy
import           Polysemy.State

import           Puzzles.Input14

import           Text.Parsec     hiding ( State )

type Bit = Int

data Instruction = Mask T.Text | Mem Int T.Text
    deriving ( Show )

maskParser :: Parsec T.Text () Instruction
maskParser = do try (string "mask = ")
                Mask . T.pack <$> count 36 (char 'X' <|> char '1' <|> char '0')

toBin n = pad $ toBin' n
  where
    toBin' 0 = "0"
    toBin' n
        | odd n = toBin' (n `div` 2) ++ ['1']
        | even n = toBin' (n `div` 2) ++ ['0']
    pad x = let x' = T.pack x in T.replicate (36 - T.length x') "0" <> x'

memParser :: Parsec T.Text () Instruction
memParser = do
    try (string "mem")
    Mem <$> ((read <$> between (string "[") (string "]") (many1 digit))
             <* string " = ") <*> (toBin . read <$> many1 digit)

instructionParser :: Parsec T.Text () Instruction
instructionParser = memParser <|> maskParser

testInput :: [T.Text]
testInput = [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
            , "mem[8] = 11"
            , "mem[7] = 101"
            , "mem[8] = 0"
            ]

parsedTestInput :: [Instruction]
parsedTestInput = either (error . show) id $
    mapM (parse instructionParser "") testInput

applyMask :: T.Text -> T.Text -> T.Text
applyMask = T.zipWith (\m' v' -> case m' of 'X' -> v'
                                            m'' -> m'')

executeInstructions :: [Instruction] -> M.Map Int T.Text
executeInstructions is = fst $ run $ runState mempty $
    runState (T.replicate 36 "X") $ go is
  where
    go :: (Member (State T.Text) r, Member (State (M.Map Int T.Text)) r)
       => [Instruction]
       -> Sem r ()
    go (Mask m : is) = do put @T.Text m
                          go is
    go (Mem ind v : is) = do m <- get @T.Text
                             modify (M.insert ind (applyMask m v))
                             go is
    go [] = pure ()

binToDec :: T.Text -> Integer
binToDec n = sum $ zipWith (*) ((2 ^) <$> [0 ..]) $
    (\case '0' -> 0
           '1' -> 1) <$> reverse (T.unpack n)

solvePart1 :: [T.Text] -> Integer
solvePart1 i = sum $ binToDec <$> M.elems (executeInstructions parsed)
  where
    parsed = either (error . show) id $ mapM (parse instructionParser "") i

part1 :: Integer
part1 = solvePart1 input

transformMask2to1 :: T.Text -> [T.Text]
transformMask2to1 m = T.pack <$> go (T.unpack m)
  where
    go [] = [""]
    go ('X' : rest) =
        let next = go rest in (('0' :) <$> next) <> (('1' :) <$> next)
    go ('1' : rest) = ('1' :) <$> go rest
    go ('0' : rest) = ('X' :) <$> go rest

applyMask2 m v = (`applyMask` v) <$> transformMask2to1 m

executeInstructions2 :: [Instruction] -> M.Map Int T.Text
executeInstructions2 is = fst $ run $ runState mempty $
    runState (T.replicate 36 "X") $ go is
  where
    go :: (Member (State T.Text) r, Member (State (M.Map Int T.Text)) r)
       => [Instruction]
       -> Sem r ()
    go (Mask m : is) = do put @T.Text m
                          go is
    go (Mem ind v : is) = do
        m <- get @T.Text
        let as = M.fromList ((\i -> (fromIntegral $ binToDec i, v))
                             <$> applyMask2 m (toBin ind))
        modify @(M.Map Int T.Text) (as <>)
        go is
    go [] = pure ()

solvePart2 :: [T.Text] -> Integer
solvePart2 i = sum $ binToDec <$> M.elems (executeInstructions2 parsed)
  where
    parsed = either (error . show) id $ mapM (parse instructionParser "") i

part2 :: Integer
part2 = solvePart2 input

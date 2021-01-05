module Puzzles.Day8 ( part1, part2 ) where

import           Data.Text
import qualified Data.Vector    as V

import           Puzzles.Input8

import           Text.Parsec

data Instruction = Acc Int | Jmp Int | Nop Int
    deriving ( Show, Eq )

numParser :: Parsec Text () Int
numParser = do s <- char '-' <|> char '+'
               n <- read <$> many digit
               pure $ if s == '-' then -n else n

instructionParser :: Parsec Text () Instruction
instructionParser = choice [ Acc <$> (string "acc " *> numParser)
                           , Jmp <$> (string "jmp " *> numParser)
                           , Nop <$> (string "nop " *> numParser)
                           ]

execute :: V.Vector Instruction -> Either ([Int], Int) Int
execute is = go 0 0 (V.replicate (V.length is) False)
  where
    go acc pos visited
        | pos == V.length is = Right acc
        | visited V.! pos = Left (Prelude.filter (visited V.!) [0 ..], acc)
        | otherwise =
            let visited' = visited V.// [(pos, True)]
            in
                case is V.! pos of Nop _ -> go acc (pos + 1) visited'
                                   Acc i -> go (acc + i) (pos + 1) visited'
                                   Jmp i -> go acc (pos + i) visited'

instructions :: V.Vector Instruction
instructions = V.fromList $ either (error . show) id $
    mapM (parse instructionParser "") input

part1 :: Int
part1 = either snd (error "program terminated") $ execute instructions

part2 :: Int
part2 = go candidates
  where
    Left (vs, _) = execute instructions
    isJmpOrNop (Acc _) = False
    isJmpOrNop _ = True
    candidates = Prelude.filter (isJmpOrNop . (instructions V.!)) vs
    flipJmpOrNop (Jmp i) = Nop i
    flipJmpOrNop (Nop i) = Jmp i
    go (c : cs) = case execute $ instructions
        V.// [(c, flipJmpOrNop (instructions V.! c))] of Right a -> a
                                                         _       -> go cs


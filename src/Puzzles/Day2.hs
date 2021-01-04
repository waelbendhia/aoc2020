module Puzzles.Day2 ( part1, part2 ) where

import           Text.Parsec.Char
import           Text.Parsec      as P
import           Puzzles.Input2
import           Data.Either

data Policy = Policy { min :: Int, max :: Int, char :: Char }
    deriving ( Show, Read )

parsePolicy :: Parsec String () ( Policy, String )
parsePolicy = do
    mi <- read <$> many digit
    P.char '-'
    ma <- read <$> many digit
    space
    c <- letter
    P.char ':'
    space
    pwd <- many P.letter
    pure ( Policy mi ma c, pwd )

parserValidatorPart1 :: Parsec String () Bool
parserValidatorPart1 = uncurry validatePart1 <$> parsePolicy

validatePart1 :: Policy -> String -> Bool
validatePart1 (Policy mi ma c) p = mi <= occurences && occurences <= ma
  where
    occurences = length $ filter (c ==) p

part1 :: Int
part1 = either (error . show) id
    (length . filter id <$> mapM (parse parserValidatorPart1 "Day2") input)

validatePart2 :: Policy -> String -> Bool
validatePart2 (Policy mi ma c) p = (pos1Contains && not pos2Contains)
    || (pos2Contains && not pos1Contains)
  where
    l = length p
    pos1Contains = l >= mi && p !! (mi - 1) == c
    pos2Contains = l >= ma && p !! (ma - 1) == c
    pos1DoesNotContain = (l < mi) || p !! (mi - 1) /= c
    pos2DoesNotContain = (l < ma) || p !! (ma - 1) /= c

parserValidatorPart2 :: Parsec String () Bool
parserValidatorPart2 = uncurry validatePart2 <$> parsePolicy

part2 :: Int
part2 = either (error . show) id
    (length . filter id <$> mapM (parse parserValidatorPart2 "Day2") input)


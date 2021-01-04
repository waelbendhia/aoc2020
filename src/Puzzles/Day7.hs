module Puzzles.Day7 ( part1, part2 ) where

import           Data.Text
import           Text.Parsec
import           Data.Function
import           Data.List      ( nub )
import           Data.Maybe
import qualified Data.Map       as M
import qualified Data.Set       as S
import           Puzzles.Input7

testInput :: [ String ]
testInput
    = [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
      , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
      , "bright white bags contain 1 shiny gold bag."
      , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
      , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
      , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
      , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
      , "faded blue bags contain no other bags."
      , "dotted black bags contain no other bags."
      ]

data BagRule = BagRule { color :: Text, contents :: [ ( Int, Text ) ] }
    deriving ( Show )

instance Eq BagRule where
    (==) = (==) `on` color

instance Ord BagRule where
    (<=) = (<=) `on` color

singleBagRuleParser :: Parsec String () BagRule
singleBagRuleParser = do
    color <- (letter <|> space) `manyTill` try (string " bags contain ")
    cs <- try ([] <$ noBag) <|> try (contentParser `sepBy` string ", ")
    char '.'
    pure $ BagRule (packTrim color) cs
  where
    packTrim      = dropAround (== ' ') . pack
    noBag         = string "no other bags"
    contentParser = do a <- read @Int <$> many1 digit
                       color <- (letter <|> space) `manyTill` try
                           (string "bag" <* optional (char 's'))
                       pure ( a, packTrim color )

findCanContain :: Text -> [ BagRule ] -> S.Set Text
findCanContain c rs = S.fromList (color <$> candidates) <> mconcat
    ((\b -> findCanContain (color b) rs) <$> candidates)
  where
    candidates = Prelude.filter (Prelude.any ((== c) . snd) . contents) rs

part1 :: Int
part1 = S.size $ findCanContain "shiny gold" rules
  where
    Right rules = mapM (parse singleBagRuleParser "") input

part2 = go "shiny gold" - 1
  where
    Right rules = mapM (parse singleBagRuleParser "") input
    rulesMap    = M.fromList $ (\r -> ( color r, r )) <$> rules
    go c = let Just rc = M.lookup c rulesMap in 1 + sum
        ((\( n, c' ) -> n * go c') <$> contents rc)


{-# LANGUAGE FlexibleContexts #-}

module Puzzles.Day4 ( part1, part2 ) where

import           Data.Text        as T hiding ( filter, length )
import           Text.Parsec
import           Text.Parsec.Char
import qualified Data.Map         as M
import           Puzzles.Input4
import           Data.Maybe
import           Control.Monad

sanitizedInput :: [ String ]
sanitizedInput = unpack . T.unwords . splitOn "\n" <$> splitOn "\n\n"
    (pack input)

data PassportField = BYR | IYR | EYR | HGT | HCL | ECL | PID | CID
    deriving ( Show, Read, Eq, Ord )

passportFieldParser :: Parsec String () PassportField
passportFieldParser = choice
    [ try $ BYR <$ string "byr"
    , try $ IYR <$ string "iyr"
    , try $ EYR <$ string "eyr"
    , try $ HGT <$ string "hgt"
    , try $ HCL <$ string "hcl"
    , try $ ECL <$ string "ecl"
    , try $ PID <$ string "pid"
    , try $ CID <$ string "cid"
    ]

passportWithFieldValueParser :: Parsec String () ( PassportField, Text )
passportWithFieldValueParser = (,) <$> (passportFieldParser <* char ':')
    <*> (pack <$> many (letter <|> digit <|> char '#'))

passportFieldMapParser :: Parsec String () (M.Map PassportField Text)
passportFieldMapParser = M.fromList <$> many
    (passportWithFieldValueParser <* (() <$ space <|> eof))

data Passport
    = Passport { byr :: Text, iyr :: Text, eyr :: Text, hgt
          :: Text, hcl :: Text, ecl :: Text, pid :: Text, cid :: Maybe Text }
    deriving ( Show, Read, Eq )

passportParser :: Parsec String () (Maybe Passport)
passportParser = do
    fs <- passportFieldMapParser
    pure $ Passport <$> M.lookup BYR fs <*> M.lookup IYR fs <*> M.lookup EYR fs
        <*> M.lookup HGT fs <*> M.lookup HCL fs <*> M.lookup ECL fs
        <*> M.lookup PID fs <*> pure (M.lookup CID fs)

part1 :: Int
part1 = length $ filter isJust $ either (error . show) id $ mapM
    (parse passportParser "") sanitizedInput

boundedInt :: Int -> Int -> Parsec String () Text
boundedInt mi ma = do
    d' <- replicateM 4 digit
    let d = read d'
    when (d < mi || d > ma) (unexpected $ Prelude.unwords
                             [ d', "must be between", show mi, "and", show ma ])
    pure $ pack d'

height :: Parsec String () Text
height = do h' <- many digit
            u <- choice [ string "cm", string "in" ]
            let h = read h'
            when (u == "cm" && (h < 150 || h > 193)) (unexpected "bad height")
            when (u == "in" && (h < 59 || h > 76)) (unexpected "bad height")
            pure $ pack $ h' <> u

hairColor :: Parsec String () Text
hairColor = do char '#'
               hcl <- many $ digit <|> letter
               pure $ pack $ '#' : hcl

passportWithFieldValueParserWithValidation
    :: Parsec String () ( PassportField, Text )
passportWithFieldValueParserWithValidation = choice $ p
    <$> [ ( BYR <$ string "byr", boundedInt 1920 2002 )
        , ( IYR <$ string "iyr", boundedInt 2010 2020 )
        , ( EYR <$ string "eyr", boundedInt 2020 2030 )
        , ( HGT <$ string "hgt", height )
        , ( HCL <$ string "hcl", hairColor )
        , ( ECL <$ string "ecl"
              , pack <$> choice
                (try . string
                 <$> [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ])
              )
        , ( PID <$ string "pid", pack <$> replicateM 9 digit )
        , ( CID <$ string "cid", pack <$> many digit )
        ]
  where
    p ( fn, fv ) = do fv' <- try (fn <* char ':')
                      fn' <- fv <?> show fv'
                      pure ( fv', fn' )

passportFieldMapParserWithValidation :: Parsec String ()
    (M.Map PassportField Text)
passportFieldMapParserWithValidation = M.fromList <$> many
    (passportWithFieldValueParserWithValidation <* (() <$ space <|> eof))

passportParserWithValidation :: Parsec String () (Maybe Passport)
passportParserWithValidation = do
    fs <- passportFieldMapParserWithValidation
    pure $ Passport <$> M.lookup BYR fs <*> M.lookup IYR fs <*> M.lookup EYR fs
        <*> M.lookup HGT fs <*> M.lookup HCL fs <*> M.lookup ECL fs
        <*> M.lookup PID fs <*> pure (M.lookup CID fs)

part2 :: Int
part2 = length $ filter (either (const False) isJust)
    $ parse passportParserWithValidation "" <$> sanitizedInput


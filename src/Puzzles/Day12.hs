module Puzzles.Day12 ( part1, part2 ) where

import           Text.Parsec
import           Puzzles.Input12
import qualified Data.Text       as T

data Card = N | S | E | W
    deriving Show

parseCard :: Parsec T.Text () Card
parseCard = choice [ f N, f S, f E, f W ]
  where
    f c = c <$ string (show c)

data Rot = R | L
    deriving Show

applyRotation R N = E
applyRotation R E = S
applyRotation R S = W
applyRotation R W = N
applyRotation L N = W
applyRotation L W = S
applyRotation L S = E
applyRotation L E = N

data Instruction = Move Card Int | Rotate Rot | Forward Int
    deriving Show

parseInstruction :: Parsec T.Text () [ Instruction ]
parseInstruction = parseMove <|> parseRotation <|> parseForward
  where
    parseMove     = (: []) <$> (Move <$> parseCard <*> (read <$> many1 digit))
    parseRotation = do r <- choice [ R <$ char 'R', L <$ char 'L' ]
                       n <- read <$> many digit
                       pure $ replicate (n `div` 90) (Rotate r)
    parseForward  = (: []) . Forward <$> (char 'F' *> (read <$> many1 digit))

executeInstructions is = go is E 0 0
  where
    go [] d x y = ( d, x, y )
    go (Move N dy : is) d x y = go is d x (y + dy)
    go (Move S dy : is) d x y = go is d x (y - dy)
    go (Move E dx : is) d x y = go is d (x + dx) y
    go (Move W dx : is) d x y = go is d (x - dx) y
    go (Rotate r : is) d x y = go is (applyRotation r d) x y
    go (Forward i : is) d x y = go (Move d i : is) d x y

testInput = concat $ either (error . show) id $ mapM (parse parseInstruction "")
    [ "F10", "N3", "F7", "R90", "F11" ]

part1 = abs x + abs y
  where
    ( _, x, y ) = executeInstructions is
    is          = concat $ either (error . show) id $ mapM
        (parse parseInstruction "") input

executeInstructions' is = go is ( 10, 1 ) 0 0
  where
    go (Move c a : is) ( wx, wy ) x y = go is
        (case c of
             N -> ( wx, wy + a )
             S -> ( wx, wy - a )
             E -> ( wx + a, wy )
             W -> ( wx - a, wy )) x y
    go (Rotate R : is) ( wx, wy ) x y = go is ( wy, -wx ) x y
    go (Rotate L : is) ( wx, wy ) x y = go is ( -wy, wx ) x y
    go (Forward i : is) ( wx, wy ) x y = go is ( wx, wy ) (x + i * wx)
        (y + i * wy)
    go [] _ x y = ( x, y )

part2 = abs x + abs y
  where
    ( x, y ) = executeInstructions' is
    is       = concat $ either (error . show) id $ mapM
        (parse parseInstruction "") input

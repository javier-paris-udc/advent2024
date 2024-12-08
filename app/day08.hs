module Main where

import AoC                (applyInput)
import Control.Arrow      ((>>>))
import Data.Function      (on)
import Data.List          (groupBy, nub, sortOn)
import Text.Parsec        ((<|>), alphaNum, char, many1, newline, sepEndBy1)
import Text.Parsec.String (Parser)

type Coord = (Int, Int)

data Antenna = Antenna { freq :: Char, loc :: Coord } deriving (Show, Eq)
data Map = Map { size :: (Int, Int), antennas :: [Antenna] } deriving (Show, Eq)


inside :: (Int, Int) -> Coord -> Bool
inside (sizeX, sizeY) (x, y) = x >= 0 && x <= sizeX && y >= 0 && y <= sizeY

(.+.) :: Coord -> Coord -> Coord
infixl 6 .+.
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)


(.-.) :: Coord -> Coord -> Coord
infixl 6 .-.
(x1, y1) .-. (x2, y2) = (x1 - x2, y1 - y2)


(.*) :: Int -> Coord -> Coord
infixr 7 .*
d .* (x, y) = (d * x, d * y)


antinodesP2 :: (Int, Int) -> (Coord, Coord) -> [Coord]
antinodesP2 size (c1, c2) =
    takeWhile (inside size) (map (\n -> c1 .+. n .* diff) [0..])
  ++takeWhile (inside size) (map (\n -> c1 .-. n .* diff) [1..])
  where
    diff = c1 .-. c2


antinodesP1 :: (Int, Int) -> (Coord, Coord) -> [Coord]
antinodesP1 size (c1, c2) =
    filter (inside size) [2 .* c1 .-. c2, 2 .* c2 .-. c1]


allAntinodes :: ((Coord, Coord) -> [Coord]) -> [Coord] -> [Coord]
allAntinodes antinodeFun antennas = concatMap antinodeFun pairs
  where
    pairs = [(c1, c2) | c1 <- antennas, c2 <- antennas, c1 < c2]


solve :: ((Coord, Coord) -> [Coord]) -> [Antenna] -> Int
solve antinodeFun =
    sortOn freq
    >>> groupBy ((==) `on` freq)
    >>> concatMap (allAntinodes antinodeFun . map loc)
    >>> nub
    >>> length


solveP2 :: Map -> Int
solveP2 m = solve (antinodesP2 (size m)) (antennas m)


solveP1 :: Map -> Int
solveP1 m = solve (antinodesP1 (size m)) (antennas m)


antennasP :: Parser Map
antennasP = do
    rows <- many1 (alphaNum <|> char '.') `sepEndBy1` newline
    let numbered = [Antenna {freq = c, loc = (x, y)} | (x, r) <- zip [0..] rows, (y, c) <- zip [0..] r]
        sizeX = maximum $ map (fst . loc) numbered
        sizeY = maximum $ map (snd . loc) numbered
    return $ Map {size = (sizeX, sizeY), antennas = filter ((/= '.') . freq) numbered}


main :: IO ()
main = applyInput antennasP solveP1 solveP2
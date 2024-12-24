{-# LANGUAGE TypeFamilies #-}
module Main where

import AoC           (applyInput)
import Text.Parsec   (char, digit, many1, sepEndBy1, spaces)
import Control.Arrow ((>>>))
import Data.MemoTrie ((:->:), HasTrie (..), Reg, enumerateGeneric, memoFix, trieGeneric, untrieGeneric)
import GHC.Generics  (Generic)


type Coord = (Int, Int)
data Move = L | R | U | D | Press deriving (Show, Eq, Generic)

instance HasTrie Move where
  newtype (Move :->: b) = MoveTrie { unMoveTrie :: Reg Move :->: b }
  trie = trieGeneric MoveTrie
  untrie = untrieGeneric unMoveTrie
  enumerate = enumerateGeneric unMoveTrie


shortestNumPad :: Coord -> Coord -> [[Move]]
shortestNumPad (i1, j1) (i2, j2)
    | j1 == 0 && i2 == 3 = [replicate j2 R ++ replicate (i2 - i1) D ++ [Press]]
    | i1 == 3 && j2 == 0 = [replicate (abs (i1 - i2)) U ++ replicate j1 L ++ [Press]]
    | i1 == i2           = [replicate (abs (j2 - j1)) horDir ++ [Press]]
    | j1 == j2           = [replicate (abs (i2 - i1)) verDir ++ [Press]]
    | otherwise = [replicate (abs (j2 - j1)) horDir ++ replicate (abs (i2 - i1)) verDir ++ [Press],
                   replicate (abs (i2 - i1)) verDir ++ replicate (abs (j2 - j1)) horDir ++ [Press]]
  where
    horDir = if j1 > j2 then L else R
    verDir = if i1 > i2 then U else D


shortestDirPad :: Coord -> Coord -> [[Move]]
shortestDirPad (i1, j1) (i2, j2)
    | i1 == 0 && j2 == 0 = [D : replicate j1 L ++ [Press]]
    | j1 == 0 && i2 == 0 = [replicate j2 R ++ [U, Press]]
    | i1 == i2           = [replicate (abs (j2 - j1)) horDir ++ [Press]]
    | j1 == j2           = [replicate (abs (i2 - i1)) verDir ++ [Press]]
    | otherwise = [replicate (abs (i2 - i1)) verDir ++ replicate (abs (j2 - j1)) horDir ++ [Press],
                   replicate (abs (j2 - j1)) horDir ++ replicate (abs (i2 - i1)) verDir ++ [Press]]
  where
    horDir = if j1 > j2 then L else R
    verDir = if i1 > i2 then U else D


codeToCoords :: Int -> Coord
codeToCoords n = case n of
    0 -> (3, 1)
    1 -> (2, 0)
    2 -> (2, 1)
    3 -> (2, 2)
    4 -> (1, 0)
    5 -> (1, 1)
    6 -> (1, 2)
    7 -> (0, 0)
    8 -> (0, 1)
    9 -> (0, 2)
    _ -> undefined


moveToCoord :: Move -> Coord
moveToCoord m = case m of
    L     -> (1, 0)
    R     -> (1, 2)
    U     -> (0, 1)
    D     -> (1, 1)
    Press -> (0, 2)


padCoordsToMoves :: (([Coord], [Move]) -> Int) -> [Coord] -> [Coord] -> Int
padCoordsToMoves _ [] _ = undefined
padCoordsToMoves padRobot !(c1:rs) !coords = case coords of
    (c2:cs) -> case shortestDirPad c1 c2 of
                    [ml] -> padRobot (rs, ml) + padCoordsToMoves padRobot (c2:map (const (0, 2)) rs) cs
                    [ml1, ml2] ->
                        let moves1 = padRobot (rs, ml1)
                            moves2 = padRobot (rs, ml2)
                            rest   = padCoordsToMoves padRobot (c2:map (const (0,2)) rs) cs
                        in min moves1 moves2 + rest
                    _ -> undefined
    _ -> 0


padRobotMoves :: [Coord] -> [Move] -> Int
padRobotMoves rbts mvs = memoFix go (rbts, mvs)
  where
    go _ ([], moves) = length moves
    go recGo (robots, moves) = padCoordsToMoves recGo robots (map moveToCoord moves)


codeRobotMoves :: Int -> [Coord] -> Int
codeRobotMoves nRobots coords = case coords of
    c1 : c2 : cs -> case shortestNumPad c1 c2 of
                [ml] -> padRobotMoves (replicate nRobots (0, 2)) ml + codeRobotMoves nRobots (c2:cs)
                [ml1, ml2] ->
                    let moves1    = padRobotMoves (replicate nRobots (0, 2)) ml1
                        moves2    = padRobotMoves (replicate nRobots (0, 2)) ml2
                        restMoves = codeRobotMoves nRobots (c2:cs)
                    in min moves1 moves2 + restMoves
                _ -> undefined
    _ -> 0


solve :: Int -> [[Int]] -> Int
solve robots = sum . map (\n -> steps n * num n)
  where
    steps = map codeToCoords
        >>> (++[(3, 2)])
        >>> ((3,2) :)
        >>> codeRobotMoves robots

    num = foldl' (\acc x -> acc * 10 + x) 0


main :: IO ()
main = applyInput (many1 (read . (:[]) <$> digit) `sepEndBy1` (char 'A' *> spaces)) (solve 2) (solve 25)
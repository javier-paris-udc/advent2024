{-# LANGUAGE DeriveGeneric #-}
module Main where


import GHC.Generics (Generic)
import AoC (applyInput)
import Data.Array (Array, array, inRange, bounds, (!), (//))
import Text.Parsec.String (Parser)
import Data.Bifunctor (second)
import Data.Foldable (find)
import Text.Parsec (many1, newline, oneOf, sepEndBy1)
import qualified Data.HashSet as Set
import Data.Hashable (Hashable)

type Coord = (Int, Int)
type Board = Array Coord Bool
data Dir = N | E | S | W deriving (Show, Eq, Generic)

instance Hashable Dir

dirFun :: Dir -> Coord -> Coord
dirFun N (x, y) = (x-1, y)
dirFun S (x, y) = (x+1, y)
dirFun W (x, y) = (x, y-1)
dirFun E (x, y) = (x, y+1)


rotate :: Dir -> Dir
rotate d = case d of
    N -> E
    E -> S
    S -> W
    W -> N


-- This is almost the same as walk, but we only store and check the visited positions
-- when we bump into an obstacle to speed up the checks
isLoop :: Coord -> Dir -> Board -> Set.HashSet (Coord, Dir) -> Bool
isLoop pos dir board visited
    | bounds board `inRange` newPos =
        if board ! newPos then
            Set.member (newPos, dir) visited || isLoop pos (rotate dir) board (Set.insert (newPos, dir) visited)
        else isLoop newPos dir board visited
    | otherwise = False
  where
      newPos = dirFun dir pos


walk :: Coord -> Dir -> Board -> Set.HashSet (Coord, Dir) -> Set.HashSet Coord
walk pos dir board visited
    | bounds board `inRange` newPos =
        if board ! newPos then walk pos (rotate dir) board visited
        else walk newPos dir board (Set.insert (newPos, dir) visited)
    | otherwise = Set.map fst visited
  where
    newPos = dirFun dir pos


solveP2 :: (Coord, Dir, Board) -> Int
solveP2 (pos, dir, board) =
    length $ filter (\p -> isLoop pos dir (board // [(p, True)]) Set.empty) obstaclePositions
  where
    obstaclePositions = Set.toList $ Set.delete pos $ walk pos dir board (Set.singleton (pos, dir))


solveP1 :: (Coord, Dir, Board) -> Int
solveP1 (pos, dir, board) = Set.size $ walk pos dir board (Set.singleton (pos, dir))


boardP :: Parser (Coord, Dir, Board)
boardP = do
    rows <- many1 (oneOf ".#^v><") `sepEndBy1` newline
    let numbered = [((x, y), v) | (x, r) <- zip [0..] rows, (y, v) <- zip [0..] r]
        board = map (second (== '#')) numbered
        sizex = maximum $ map (fst . fst) board
        sizey = maximum $ map (snd . fst) board
    case find ((`elem` "^v><"). snd) numbered of
        Nothing ->
            fail "No guard"
        Just (guardPos, guardDir) ->
            return (guardPos, charToDir guardDir, array ((0, 0), (sizex, sizey)) board)
  where
    charToDir c = case c of
        '^' -> N
        'v' -> S
        '<' -> W
        '>' -> E
        _   -> undefined


main :: IO ()
main = applyInput boardP solveP1 solveP2
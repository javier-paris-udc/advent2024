{-# LANGUAGE DeriveGeneric #-}
module Main where


import           AoC                         (applyInput)
import           Control.Parallel.Strategies (parList, rseq, using)
import           Data.Array                  (Array, (!), (//), array, bounds, inRange)
import           Data.Bifunctor              (second)
import           Data.Function               ((&))
import           Data.Hashable               (Hashable)
import qualified Data.HashSet                as Set
import           GHC.Generics                (Generic)
import           Text.Parsec                 (many1, newline, oneOf, sepEndBy1)
import           Text.Parsec.String          (Parser)

type Coord = (Int, Int)
type Board = Array Coord Bool
data Dir = N | E | S | W deriving (Show, Eq, Generic)

instance Hashable Dir

move :: Dir -> Coord -> Coord
move N (x, y) = (x-1, y)
move S (x, y) = (x+1, y)
move W (x, y) = (x, y-1)
move E (x, y) = (x, y+1)


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
      newPos = move dir pos


walk :: Coord -> Dir -> Board -> Set.HashSet Coord -> Set.HashSet Coord
walk pos dir board visited
    | bounds board `inRange` newPos =
        if board ! newPos then walk pos (rotate dir) board visited
        else walk newPos dir board (Set.insert newPos visited)
    | otherwise = visited
  where
    newPos = move dir pos


solveP2 :: (Coord, Dir, Board) -> Int
solveP2 (pos, dir, board) =
    (map (\p -> isLoop pos dir (board // [(p, True)]) Set.empty) obstaclePositions `using` parList rseq)
    & filter id
    & length
  where
    obstaclePositions = Set.toList $ Set.delete pos $ walk pos dir board (Set.singleton pos)


solveP1 :: (Coord, Dir, Board) -> Int
solveP1 (pos, dir, board) = Set.size $ walk pos dir board (Set.singleton pos)


boardP :: Parser (Coord, Dir, Board)
boardP = do
    rows <- many1 (oneOf ".#^v><") `sepEndBy1` newline
    let numbered = [((x, y), v) | (x, r) <- zip [0..] rows, (y, v) <- zip [0..] r]
        board    = map (second (== '#')) numbered
        sizex    = maximum $ map (fst . fst) board
        sizey    = maximum $ map (snd . fst) board
    case filter ((`elem` "^v><"). snd) numbered of
        [(guardPos, guardDir)] ->
            return (guardPos, charToDir guardDir, array ((0, 0), (sizex, sizey)) board)
        [] ->
            fail "No guard"
        _ ->
            fail "There are several guards"
  where
    charToDir c = case c of
        '^' -> N
        'v' -> S
        '<' -> W
        '>' -> E
        _   -> undefined


main :: IO ()
main = applyInput boardP solveP1 solveP2
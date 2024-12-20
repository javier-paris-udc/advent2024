module Main where

import           AoC                (applyInput)
import           Data.Array         (Array, (!), array)
import           Data.Bifunctor     (first, second)
import           Data.Foldable      (find)
import           Data.Function      ((&))
import           Data.List          (sortOn)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromJust)
import           Text.Parsec        (many1, oneOf, sepEndBy1, spaces)
import           Text.Parsec.String (Parser)


type Coord = (Int, Int)
type Board = Array Coord Bool
type Path  = Map.Map Coord Int -- Coord to step number


allDirs :: [Coord -> Coord]
allDirs = [first (+1), first (subtract 1), second (+1), second (subtract 1)]


findCheats :: Int -> Path -> [Int]
findCheats jump path = jumps (sortOn snd $ Map.toList path)
  where
    jumps [] = []
    jumps ((pos, n):ps) =
        let posJumps = filter (\(p, _) -> distance pos p <= jump) ps
                        & map (uncurry (computeSavings pos n))
                        & filter (> 0)
        in posJumps ++ jumps ps

    distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

    computeSavings pos n pos2 n2 = n2 - n - distance pos2 pos


explore :: Coord -> Coord -> Array Coord Bool -> [Coord]
explore start end board = doExplore start []
  where
    doExplore pos path
        | pos == end = reverse (end : path)
        | otherwise =
            case filter (isNextPos path) (map ($ pos) allDirs) of
                nextPos:_ -> doExplore nextPos (pos : path)
                _         -> undefined

    isNextPos []          p = not (board ! p)
    isNextPos (lastPos:_) p = not (board ! p) && p /= lastPos


solve :: Board -> Coord -> Coord -> Int -> Int
solve board start end steps =
      explore start end board
    & (`zip` [0..])
    & Map.fromList
    & findCheats steps
    & filter (>=100)
    & length


solveP2 :: (Board, Coord, Coord) -> Int
solveP2 (board, start, end) = solve board start end 20


solveP1 :: (Board, Coord, Coord) -> Int
solveP1 (board, start, end) = solve board start end 2


boardP :: Parser (Board, Coord, Coord)
boardP = do
    rows <- many1 (oneOf "#.SE") `sepEndBy1` spaces
    let numbered = [((x,y), c) | (x, r) <- zip [0..] rows, (y, c) <- zip [0..] r]
        start = fst $ fromJust $ find ((=='S') . snd) numbered
        end = fst $ fromJust $ find ((=='E') . snd) numbered
        maxX = maximum $ map (fst . fst) numbered
        maxY = maximum $ map (snd . fst) numbered
        arr = array ((0, 0), (maxX, maxY)) (map (second (=='#')) numbered)
    return (arr , start, end)


main :: IO ()
main = applyInput boardP solveP1 solveP2
module Main where

import           AoC                (applyInput)
import           Data.Bifunctor     (second)
import           Data.Foldable      (find)
import           Data.Function      ((&))
import           Data.Functor       (($>))
import qualified Data.Map           as Map
import           Data.Maybe         (fromMaybe)
import           Text.Parsec        (char, choice, many1, oneOf, sepEndBy1, spaces)
import           Text.Parsec.String (Parser)


type Coord = (Int, Int)
data Tile = Wall | Box deriving (Show, Eq)
type Warehouse = Map.Map Coord Tile
data Move = N | S | E | W deriving (Show, Eq, Ord)


nextCoord :: Coord -> Move -> Coord
nextCoord (x, y) m = case m of
    N -> (x, y - 1)
    S -> (x, y + 1)
    E -> (x + 1, y)
    W -> (x - 1, y)

nextCoordP2 :: Coord -> Move -> (Coord, Coord)
nextCoordP2 (x, y) m = case m of
    N -> ((x, y - 1), (x - 1, y - 1))
    S -> ((x, y + 1), (x - 1, y + 1))
    E -> ((x + 1, y), (x + 2, y))
    W -> ((x - 1, y), (x - 2, y))


score :: Coord -> Int
score (x, y) = y * 100 + x


doMoveP1 :: (Warehouse, Coord) -> Move -> (Warehouse, Coord)
doMoveP1 (whs, rbt) move =
    case whs Map.!? rbtNext of
        Nothing   -> (whs, rbtNext)
        Just Wall -> (whs, rbt)
        Just Box  -> case moveBox rbtNext of
            Just finalBoxDst ->
                (Map.alter (const (Just Box)) finalBoxDst $ Map.alter (const Nothing) rbtNext whs, rbtNext)
            Nothing -> (whs, rbt)
  where
    rbtNext = nextCoord rbt move

    moveBox box =
        let dst = nextCoord box move in case whs Map.!? dst of
            Nothing   -> Just dst
            Just Wall -> Nothing
            Just Box  -> moveBox dst

moveBoxP2 :: Coord -> Move -> Warehouse -> Maybe Warehouse
moveBoxP2 box move whs
    | move `elem` [N, S] =
        let next@(nx, ny) = nextCoord box move in
         if whs Map.!? next == Just Wall || whs Map.!? (nx + 1, ny) == Just Wall then Nothing
         else let boxes = filter isBox [(nx + x, ny) | x <- [-1 .. 1]]
                  whs2  = Map.delete box whs
              in Map.insert next Box <$> foldl' (\w c -> w >>= moveBoxP2 c move) (Just whs2) boxes
    | otherwise =
        let (next, next2) = nextCoordP2 box move in
         if whs Map.!? afterBox next next2 move == Just Wall then Nothing
         else if whs Map.!? next2 == Just Box then Map.insert next Box <$> moveBoxP2 next2 move (Map.delete box whs)
         else Just $ Map.insert next Box $ Map.delete box whs
  where
    isBox c = whs Map.!? c == Just Box

    afterBox next next2 E = next2
    afterBox next next2 W = next


doMoveP2 :: (Warehouse, Coord) -> Move -> (Warehouse, Coord)
doMoveP2 (whs, rbt) move =
    case (whs Map.!? rbtNext, whs Map.!? rbtNext2, move) of
        (Nothing, _, E) -> (whs, rbtNext)
        (Nothing, Just Box, _) -> case moveBoxP2 rbtNext2 move whs of
            Nothing -> (whs, rbt)
            Just newWhs -> (newWhs, rbtNext)
        (Nothing, _, _) -> (whs, rbtNext)
        (Just Wall, _, _) -> (whs, rbt)
        (Just Box, _, _) -> case moveBoxP2 rbtNext move whs of
            Nothing -> (whs, rbt)
            Just newWhs -> (newWhs, rbtNext)
  where
    (rbtNext, rbtNext2) = nextCoordP2 rbt move


solve :: Warehouse -> Coord -> [Move] -> ((Warehouse, Coord) -> Move -> (Warehouse, Coord)) -> Int
solve whs rbt moves moveFun =
    foldl' moveFun (whs, rbt) moves
    & fst
    & Map.filter (== Box)
    & Map.toList
    & map (score . fst)
    & sum


solveP2 :: (Warehouse, Coord, [Move]) -> Int
solveP2 (whs, (rbtX, rbtY), moves) = solve wideWhs wideRbt moves doMoveP2
  where
    wideWhs = foldl' widen Map.empty $ Map.toList whs
    wideRbt = (rbtX * 2, rbtY)

    widen m ((x, y), t) = case t of
        Wall -> Map.insert (x * 2 + 1, y) Wall $ Map.insert (x * 2, y) Wall m
        Box  -> Map.insert (x * 2, y) Box m


solveP1 :: (Warehouse, Coord, [Move]) -> Int
solveP1 (whs, rbt, moves) = solve whs rbt moves doMoveP1


warehouseP :: Parser (Warehouse, Coord, [Move])
warehouseP = do
    (m, rbt) <- mapP
    moves <- movesP
    return (m, rbt, moves)
  where
    mapP = do
        rows <- many1 (oneOf "#O@.") `sepEndBy1` spaces
        let numbered = [((x, y), c) | (y, row) <- zip [0..] rows, (x, c) <- zip [0..] row]
            robot = fromMaybe undefined $ find ((=='@') . snd) numbered
        return (Map.fromList (map (second tile) $ filter ((`elem`  "O#") . snd) numbered) , fst robot)

    tile '#' = Wall
    tile 'O' = Box
    tile _ = undefined

    movesP = fmap concat $ many1 moveP `sepEndBy1` spaces

    moveP =
        choice
            [char '^' $> N
            ,char 'v' $> S
            ,char '>' $> E
            ,char '<' $> W
            ]


main :: IO ()
main = applyInput warehouseP solveP1 solveP2
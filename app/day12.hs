module Main where

import           AoC                (applyInput)
import           Data.Array         (Array, (!), array, bounds, indices, inRange)
import           Data.Bifunctor     (bimap, first, second)
import           Data.Function      ((&), on)
import           Data.List          (groupBy, sort)
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set
import           Data.Tuple         (swap)
import           Text.Parsec        (alphaNum, many1, sepEndBy1, spaces)
import           Text.Parsec.String (Parser)

type Coord = (Int, Int)
type PlotMap = Array Coord Char


plotsNextTo :: Coord -> [Coord]
plotsNextTo (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]


extract :: Either a a -> a
extract (Left x) = x
extract (Right x) = x


regions :: PlotMap -> Map.Map Coord Int
regions plotMap = buildRegions [0..] (indices plotMap) Map.empty
  where
    buildRegions _ids [] m = m
    buildRegions ids@(i : is) (plot : plots) m
        | plot `Map.member` m = buildRegions ids plots m
        | otherwise = buildRegions is plots (flow i [plot] (plotMap ! plot) m)
    buildRegions [] _ _ = undefined -- prevent the non-exhaustive error. The list is inifinite, so it shouldn't happen

    flow _ [] _ m = m
    flow i (plot : plots) c m
        | plot `Map.member` m = flow i plots c m
        | otherwise = flow i (neighbors plot c ++ plots) c (Map.insert plot i m)

    neighbors plot c =
        filter
            (\p -> inRange (bounds plotMap) p && plotMap ! p == c)
            (plotsNextTo plot)


toRegionMap :: Map.Map Coord Int -> Map.Map Int (Set.Set Coord)
toRegionMap = foldr (\(plot, reg) -> Map.alter (addPlot plot) reg) Map.empty . Map.toList
  where
    addPlot plot Nothing = Just $ Set.singleton plot
    addPlot plot (Just s) = Just $ Set.insert plot s


fenceP2 :: Set.Set Coord -> Int
fenceP2 plots = Set.size plots * (sides horizontal + sides vertical)
  where
    plotList = Set.toList plots

    sides sideFun =
        concatMap sideFun plotList
        & sort
        & groupBy ((==) `on` bimap fst fst)
        & map (countSides . map (snd . extract))
        & sum

    countSides [] = 0
    countSides (x:xs) = 1 + countSides (dropDiff1 x xs)

    dropDiff1 _ [] = []
    dropDiff1 x (x1:xs)
        | abs (x - x1) == 1 = dropDiff1 x1 xs
        | otherwise = x1:xs

    -- Both functions will leave the coordinate on which we are checking lines on the first coordinate.
    -- That way we can check for lines without having to write two functions that check the two coordinates swapped
    horizontal = checkCoords (first . (+))
    vertical   = map (bimap swap swap) . checkCoords (second . (+))

    checkCoords add c
        | add (-1) c `Set.notMember` plots && add 1 c `Set.notMember` plots = [Left c , Right c]
        | add (-1) c `Set.notMember` plots = [Left c]
        | add 1    c `Set.notMember` plots = [Right c]
        | otherwise = []


fenceP1 :: Set.Set Coord -> Int
fenceP1 plots = Set.size plots * sum (map borderSides (Set.toList plots))
  where
    borderSides plot = length $ filter (`Set.notMember` plots) $ plotsNextTo plot


solve :: (Set.Set Coord -> Int) ->  Map.Map Coord Int -> Int
solve fence = sum . fmap fence . toRegionMap


plotsP ::  Parser (Map.Map Coord Int)
plotsP = do
    rows <- many1 alphaNum `sepEndBy1` spaces
    let numbered = [((x, y), c) | (x, r) <- zip [0..] rows, (y, c) <- zip [0..] r]
        sizeX = maximum (map (fst . fst) numbered)
        sizeY = maximum (map (snd . fst) numbered)
    return $ regions $ array ((0, 0), (sizeX, sizeY)) numbered


main :: IO ()
main = applyInput plotsP (solve fenceP1) (solve fenceP2)
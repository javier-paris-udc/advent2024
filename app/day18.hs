module Main where

import           AoC           (applyInput, commaSepP, intP)
import           Text.Parsec   (sepEndBy1, spaces)
import qualified Data.Sequence as Seq
import qualified Data.Set      as Set
import           Data.Sequence (Seq(..))
import           Data.Function ((&))
import           Data.Maybe    (fromJust, isJust)

type Coord = (Int, Int)
data Border = N | S | E | W deriving (Show, Eq, Ord)


nextTo :: Coord -> [Coord]
nextTo (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]


insertQ :: Coord -> Int -> Seq.Seq (Coord, Int) -> Seq.Seq (Coord, Int)
insertQ pos cost prioQ = Seq.insertAt (findIdx 0 (Seq.length prioQ - 1)) (pos, cost) prioQ
  where
    findIdx st end
        | st > end = st
        | otherwise =
            let half = (st + end) `div` 2
                costHalf = snd (Seq.index prioQ half)
            in if cost < costHalf then findIdx st (half - 1)
               else if cost > costHalf then findIdx (half + 1) end
               else half


dijkstra :: Int
         -> Int
         -> Seq.Seq (Coord, Int)
         -> Set.Set Coord
         -> Set.Set Coord
         -> Coord
         -> Maybe Int
dijkstra maxX maxY pq corrupt visited end =
    case pq of
        Seq.Empty -> Nothing
        (pos, cost) :<| rest
            | pos == end -> Just cost
            | pos `Set.member` visited -> dijkstra maxX maxY rest corrupt visited end
            | otherwise  ->
                let dsts = filter valid (nextTo pos)
                         & map (, 1)
                    newPq = foldl' (\s (p, c) -> insertQ p (cost + c) s) rest dsts
                in dijkstra maxX maxY newPq corrupt (Set.insert pos visited) end
  where
    valid pos@(x, y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY
                    && Set.notMember pos visited && Set.notMember pos corrupt


solveP2 :: [Coord] -> Coord
solveP2 corrupt = corrupt !! (binSearch 1024 (length corrupt - 1) -1)
  where
    hasSol n = isJust $ dijkstra 70 70 (Seq.singleton ((0,0), 0)) (Set.fromList (take n corrupt)) Set.empty (70, 70)
    binSearch l r
        | l > r = l
        | otherwise =
            let half = (l + r) `div` 2 in
            if hasSol half then binSearch (half + 1) r
            else binSearch l (half - 1)


solveP1 :: [Coord] -> Int
solveP1 corrupt =
    fromJust $ dijkstra 70 70 (Seq.singleton ((0,0), 0)) (Set.fromList (take 1024 corrupt)) Set.empty (70, 70)


main :: IO ()
main = applyInput (coordP `sepEndBy1` spaces) solveP1 solveP2
  where
    coordP = liftA2 (,) intP (commaSepP *> intP)
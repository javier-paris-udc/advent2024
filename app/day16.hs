module Main where

import           AoC                (applyInputSWith, fst3, snd3)
import           Data.Array         (Array, (!), array, indices)
import           Data.Bifunctor     (second)
import           Data.Foldable      (find)
import           Data.Function      ((&))
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromJust)
import qualified Data.Set           as Set
import           Data.Sequence      (Seq((:<|)))
import qualified Data.Sequence      as Seq
import           Text.Parsec        (many1, oneOf, sepEndBy1, spaces)
import           Text.Parsec.String (Parser)

type Coord = (Int, Int)
type Board = Array Coord Bool
data Dir = N | S | E | W deriving (Show, Eq, Ord)
type Graph = Map.Map Node [(Node, Int, Set.Set Coord)]

-- We are going to search using dijkstra on a graph on which the nodes
-- are the intesections and the direction we entered them from. This is
-- to account for the difference in cost depending on the rotations we have to make
type Node = (Coord, Dir)


isPath :: Board -> Coord -> Bool
isPath b pos = not $ b ! pos


insertQ :: Node -> Int -> Set.Set Coord -> Seq.Seq (Node, Int, Set.Set Coord) -> Seq.Seq (Node, Int, Set.Set Coord)
insertQ node cost path prioQ = Seq.insertAt (findIdx 0 (Seq.length prioQ - 1)) (node, cost, path) prioQ
  where
    findIdx st end
        | st > end = st
        | otherwise =
            let half = (st + end) `div` 2
                costHalf = snd3 (Seq.index prioQ half)
            in if cost < costHalf then findIdx st (half - 1)
               else if cost > costHalf then findIdx (half + 1) end
               else half


go :: Dir -> Coord -> Coord
go N (x, y) = (x - 1, y)
go S (x, y) = (x + 1, y)
go E (x, y) = (x, y + 1)
go W (x, y) = (x, y - 1)


oppositeDir :: Dir -> Dir
oppositeDir N = S
oppositeDir S = N
oppositeDir E = W
oppositeDir W = E


turnCost :: Dir -> Dir -> Int
turnCost d1 d2
    | d1 == d2 = 0
    | d1 == oppositeDir d2 = 2000
    | otherwise = 1000


dijkstra :: Seq.Seq (Node, Int, Set.Set Coord)
         -> Graph
         -> Map.Map Node Int
         -> Coord
         -> Maybe (Int, Set.Set Coord)
         -> (Int, Set.Set Coord)
dijkstra pq graph visited end sol =
    case pq of
        Seq.Empty -> fromJust sol
        (node@(pos, _), cost, path) :<| rest
            | overCost cost -> fromJust sol
            | pos == end -> dijkstra rest graph (Map.insert node cost visited) end (addSol cost path)
            | node `Map.member` visited && visited Map.! node < cost -> dijkstra rest graph visited end sol
            | otherwise  ->
                let dsts = case graph Map.!? node of
                            Nothing -> []
                            Just nextNodes -> filter (shouldVisit cost. fst3) nextNodes
                    newPq = foldl' (\s (n, c, edgePath) -> insertQ n (cost + c) (Set.union path edgePath) s) rest dsts
                in dijkstra newPq graph (Map.insert node cost visited) end sol
  where
    overCost cost = maybe False ((<cost) . fst) sol

    shouldVisit cost node = maybe True (>= cost) (visited Map.!? node)

    addSol cost path = case sol of
        Nothing -> Just (cost, path)
        Just (_, paths) -> Just (cost, Set.union paths path)


buildGraph :: Board -> Coord -> Coord -> Graph
buildGraph b start end = Map.fromListWith (++) nodesWithEdges
  where
    nodes = indices b
          & filter (isPath b)
          & map (\pos -> (pos, nextDirs pos))
          & filter isNode

    isNode (pos, dirs) = pos == start || pos == end || length dirs /= 2

    nodeSet = Set.fromList $ map fst nodes

    nodesWithEdges = do
        (pos, dirs) <- nodes
        (exitDir, (cost, dstNode, tiles)) <- map (\dir -> (dir, explore 1 (go dir pos) dir Set.empty)) dirs
        [((pos, oppositeDir dir) ,[(dstNode, cost + turnCost (oppositeDir dir) exitDir, tiles)])
         | dir <- getDirs pos dirs, dir /= exitDir]

    getDirs pos dirs
        | pos == start = [W]
        | otherwise = dirs

    nextDirs pos = filter (isPath b . (`go` pos)) [N, S, E, W]

    explore cost pos dir visited
        | pos `Set.member` nodeSet = (cost, (pos, dir), Set.insert pos visited)
        | otherwise = explore nextCost next nextDir (Set.insert pos visited)
      where
        (next, nextDir, nextCost) =
            case find (isPath b . (`go` pos)) (filter (/= oppositeDir dir) [N, S, E, W]) of
                Nothing -> undefined
                Just nextD -> (go nextD pos, nextD, cost + 1 + turnCost dir nextD)


solve :: (Board, Coord, Coord) -> (Int, Int)
solve (b, start, end) =
    let (cost, visited) = dijkstra pq graph Map.empty end Nothing
    in (cost, Set.size visited)
  where
    graph = buildGraph b start end
    pq = insertQ (start, E) 0 (Set.singleton start) Seq.Empty


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
main = applyInputSWith boardP () showSols
  where
    showSols arg = do
        let (len, nodes) = solve arg
        print len
        print nodes
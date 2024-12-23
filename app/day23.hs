module Main where

import           AoC                (applyInput)
import           Data.Function      ((&), on)
import           Data.List          (intercalate, maximumBy, sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set
import           Text.Parsec        (alphaNum, many1, sepEndBy1, spaces, string)


solveP2 :: [(String, String)] -> String
solveP2 pairs = intercalate "," maximumConn
  where
    computers = Set.toAscList $ Set.fromList $ concatMap (\(a, b) -> [a, b]) pairs
    pairSet = Set.fromList $ map (\(a, b) -> (min a b, max a b)) (map (\c -> (c, c)) computers ++ pairs)

    mutualSets = [ (x, sharedConns x) | x <- computers]

    sharedConns x = [Set.intersection (connSets Map.! x) (connSets Map.! y) | y <- computers]
                  & filter ((>3) . Set.size)

    connSet x = Set.map (\(a, b) -> if a /= x then a else b) $ Set.filter (\(a, b) -> a == x || b == x) pairSet

    connSets = Map.fromList $ map (\c -> (c, connSet c)) computers

    maximumConn = map (\(s, ls) -> (s, map (\l -> (NE.head l, length l)) $ NE.group $ sort $ ls)) mutualSets
                & maximumBy (compare `on` (maximum . map snd) . snd)
                & snd
                & maximumBy (compare `on` snd)
                & fst
                & Set.toAscList


solveP1 :: [(String, String)] -> Int
solveP1 pairs = findTriplets computers
  where
    computers = Set.toAscList $ Set.fromList $ concatMap (\(a,b) -> [a, b]) pairs
    pairSet = Set.fromList $ map (\(a, b) -> (min a b, max a b)) pairs

    findTriplets [] = 0
    findTriplets (x:xs) = findWith x xs + findTriplets xs

    findWith _ [] = 0
    findWith x (y:ys)
        | (x, y) `Set.member` pairSet = findBothWith x y ys + findWith x ys
        | otherwise = findWith x ys

    findBothWith _ _ [] = 0
    findBothWith x y (z:zs)
        | (x, z) `Set.member` pairSet && (y, z) `Set.member` pairSet
           && (hasT x || hasT y || hasT z)= 1 + findBothWith x y zs
        | otherwise = findBothWith x y zs

    hasT [] = False
    hasT (x:_) = x == 't'

main :: IO ()
main = applyInput (connP `sepEndBy1` spaces) solveP1 solveP2
  where
    connP = liftA2 (,) (many1 alphaNum) (string "-" >> many1 alphaNum)
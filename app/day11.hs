module Main where

import           AoC                (applyInput, intP)
import qualified Data.IntMap.Strict as Map
import           Text.Parsec        (sepEndBy1, spaces)


digits :: Int -> Int
digits x
    | x < 10 = 1
    | otherwise = 1 + digits (x `div` 10)


split :: Int -> (Int, Int)
split x = (x `div` half_digits, x `mod` half_digits)
  where
    half_digits = 10 ^ (digits x `div` 2)


applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n - 1) f (f x)


blink :: Int -> [Int] -> Int
blink steps = sum . applyN steps (Map.foldlWithKey' blinkNum Map.empty) . toMap
  where
    toMap :: [Int] -> Map.IntMap Int
    toMap = Map.fromListWith (+) . map (, 1)

    blinkNum :: Map.IntMap Int -> Int -> Int -> Map.IntMap Int
    blinkNum m n count
        | n == 0 = Map.insertWith (+) 1 count m
        | even (digits n) =
            let (n1, n2) = split n in
                Map.insertWith (+) n1 count (Map.insertWith (+) n2 count m)
        | otherwise = Map.insertWith (+) (n * 2024) count m


solveP1 :: [Int] -> Int
solveP1 =  blink 25


solveP2 :: [Int] -> Int
solveP2 = blink 75


main :: IO ()
main = applyInput (intP `sepEndBy1` spaces) solveP1 solveP2
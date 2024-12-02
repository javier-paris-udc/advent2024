module Main where

import AoC                (applyInput, intP)
import Control.Arrow      ((>>>))
import Data.Bifunctor     (bimap)
import Data.Function      ((&))
import Data.List          (sort)
import Data.List.NonEmpty (head, group)
import Prelude hiding     (head)
import Text.Parsec        (newline, sepEndBy, spaces)
import Text.Parsec.String (Parser)


solveP2 :: ([Int], [Int]) -> Int
solveP2 (l1, l2) = timesIds (sort l1) countl2
  where
    countl2 =
          sort l2
        & group
        & map (\l -> (head l, length l))

    timesIds [] _ = 0
    timesIds _ [] = 0
    timesIds xl@(x:xs) yl@((y, times) : ys)
        | x < y = timesIds xs yl
        | x > y = timesIds xl ys
        | otherwise = x*times + timesIds xs yl


solveP1 :: ([Int], [Int]) -> Int
solveP1 =
        bimap sort sort
    >>> uncurry (zipWith (-))
    >>> map abs
    >>> sum


numListsP :: Parser ([Int], [Int])
numListsP = unzip <$> numPairP `sepEndBy` newline
  where
    numPairP = do
        n1 <- intP <* spaces
        n2 <- intP
        return (n1, n2)


main :: IO ()
main = applyInput numListsP solveP1 solveP2

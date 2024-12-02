module Main where

import AoC                (applyInput, blanksP, intP)
import Data.List.NonEmpty (NonEmpty((:|)))
import Text.Parsec        (newline, sepEndBy, sepEndBy1)
import Text.Parsec.String (Parser)


safe :: [Int] -> Bool
safe l = (all (<0) diffs || all (>0) diffs) && all ((<=3) . abs) diffs
  where
    diffs = zipWith (-) l (drop 1 l)


solveP2 :: [[Int]] -> Int
solveP2 = length . filter (any safe . allBut1)
  where
    allBut1 = foldr (\x (y:|ys) -> (x:y):|y:map (x:) ys) ([] :| [])


solveP1 :: [[Int]] -> Int
solveP1 = length . filter safe


levelsP :: Parser [[Int]]
levelsP = (intP `sepEndBy1` blanksP) `sepEndBy` newline


main :: IO ()
main = applyInput levelsP solveP1 solveP2
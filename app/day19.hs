module Main where

import AoC                (applyInput, blanksP)
import Data.Function      ((&))
import Data.Maybe         (mapMaybe)
import Data.MemoTrie      (memoFix)
import Text.Parsec        (many1, oneOf, sepEndBy, spaces, string)
import Text.Parsec.String (Parser)


ways :: [String] -> String -> Int
ways towels = memoFix findWays
  where
    findWays _  []  = 1
    findWays go pat = mapMaybe (dropTowel pat) towels
                    & map go
                    & sum

    dropTowel st [] = Just st
    dropTowel [] _  = Nothing
    dropTowel (s:ss) (t:ts)
        | s == t    = dropTowel ss ts
        | otherwise = Nothing


solveP2 :: ([String], [String]) -> Int
solveP2 (towels, patterns) = sum $ map (ways towels) patterns


solveP1 :: ([String], [String]) -> Int
solveP1 (towels, patterns) = length $ filter ((>0) . ways towels) patterns


towelsP :: Parser ([String], [String])
towelsP = liftA2 (,) (towelP `sepEndBy` (string "," >> blanksP)) (spaces >> towelP `sepEndBy` spaces)
  where
    towelP = many1 (oneOf "wubrg")


main :: IO ()
main = applyInput towelsP solveP1 solveP2
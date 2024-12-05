module Main where

import           AoC                (applyInput, sep, intP)
import qualified Data.HashSet       as Set
import           Data.List          (sortBy)
import           Text.Parsec        (char, sepEndBy1, spaces, try)
import           Text.Parsec.String (Parser)


satisfies :: Eq a => [a] -> (a, a) -> Bool
satisfies [] _ = True
satisfies (x:xs) (a, b)
    | x == a = True
    | x == b = a `notElem` xs
    | otherwise = satisfies xs (a, b)


middle :: [a] -> a
middle l = l !! (length l `div` 2)


-- Checking the input, the rules satisfy the antisymmetric property for every update, so we can just sort them
solveP2 :: ([(Int, Int)], [[Int]]) -> Int
solveP2 (rules, updates) = sum $ map (middle . sortBy comp) notSatifying
  where
    notSatifying = filter (\u -> not (all (u `satisfies`) rules)) updates
    ruleSet      = Set.fromList rules
    comp x y
        | (x, y) `Set.member` ruleSet = LT
        | (y, x) `Set.member` ruleSet = GT
        | otherwise = EQ


solveP1 :: ([(Int, Int)], [[Int]]) -> Int
solveP1 (rules, updates) = sum $ map middle $ filter (\u -> all (u `satisfies`) rules) updates


safetyP :: Parser ([(Int, Int)], [[Int]])
safetyP = do
    rules <- try ruleP `sepEndBy1` spaces
    updates <- (intP `sepEndBy1` char ',') `sepEndBy1` spaces
    return (rules, updates)
  where
    ruleP = do
        n1 <- intP <* sep "|"
        n2 <- intP
        return (n1, n2)


main :: IO ()
main = applyInput safetyP solveP1 solveP2

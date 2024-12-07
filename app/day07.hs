module Main where

import AoC                (applyInput, blanksP, intP)
import Text.Parsec        (char, sepEndBy1, spaces)
import Text.Parsec.String (Parser)


digits :: Int -> Int
digits x
    | abs x < 10 = 1
    | otherwise  = 1 + digits (x `div` 10)


(.+.) :: Int -> Int -> Int
a .+. b = a * 10 ^ digits b + b


solvableEq :: [Int -> Int -> Int] -> Int -> [Int] -> Bool
solvableEq ops x l = case l of
    []          -> False
    [y]         -> x == y
    y:_ | y > x -> False
    y1:y2:ys    -> any (\op -> solvableEq ops x (y1 `op` y2 : ys)) ops


solve :: [Int -> Int -> Int] -> [(Int, [Int])] -> Int
solve ops = sum . map fst . filter (uncurry (solvableEq ops))


equationsP :: Parser [(Int, [Int])]
equationsP = eqP `sepEndBy1` spaces
  where
    eqP = do
        target <- intP <* char ':' <* blanksP
        vals   <- intP `sepEndBy1` blanksP
        return (target, vals)


main :: IO ()
main = applyInput equationsP (solve [(+), (*)]) (solve [(+), (*), (.+.)])
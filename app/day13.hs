{-# LANGUAGE LambdaCase #-}
module Main where

import AoC (applyInput, blanksP, intP)
import Text.Parsec.String (Parser)
import Text.Parsec (sepEndBy1, spaces, string)
import Data.Function ((&))

data Equation = Equation { a :: Int , b :: Int, prize :: Int } deriving (Show, Eq)
type Machine = (Equation, Equation)


-- brute forcing for now
solveMachine :: Machine -> Int
solveMachine (eq1, eq2) =
    [(av, bv) | av <- [0..100], bv <- [0..100], isSol av bv eq1, isSol av bv eq2]
    & map (\(av, bv) -> 3*av + bv)
    & (\case [] -> 0; l -> minimum l)

isSol av bv eq = av * a eq + bv * b eq == prize eq


-- not a general solution, but the linear equation system are all determinated or incompatible
solveP1 :: [Machine] -> Int
solveP1 = sum . map solveMachine


machinesP :: Parser [Machine]
machinesP = machineP `sepEndBy1` spaces
  where
    machineP = do
        (xa, ya) <- buttonP "A" <* spaces
        (xb, yb) <- buttonP "B" <* spaces
        xp <- string "Prize:" *> blanksP *> string "X=" *> intP
        yp <- string "," *> blanksP *> string "Y=" *> intP
        return (Equation { a = xa, b = xb, prize = xp }, Equation { a = ya, b = yb, prize = yp })

    buttonP b = do
        x <- string "Button" *> blanksP *> string b *> string ":" *> blanksP *> string "X+" *> intP
        y <- string "," *> blanksP *> string "Y+" *> intP
        return (x, y)

main :: IO ()
main = applyInput machinesP solveP1 (const '-')
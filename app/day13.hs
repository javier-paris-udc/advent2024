module Main where

import AoC (applyInput, blanksP, intP)
import Text.Parsec.String (Parser)
import Text.Parsec (sepEndBy1, spaces, string)
import Data.Ratio (denominator, (%), numerator)
import Data.Maybe (mapMaybe)
import Data.Bifunctor (bimap)

data Equation = Equation { a :: Rational, b :: Rational, prize :: Rational } deriving (Show, Eq)
type Machine = (Equation, Equation)


-- Not a general solver, but all systems of linear diophantine equations are incompatible or
-- determinated,- so we can ignore the indeterminated case
solveMachine :: Machine -> Maybe (Integer, Integer)
solveMachine (eq1, eq2)
    | denominator valA == 1 && denominator valB == 1 = Just (numerator valA, numerator valB)
    | otherwise = Nothing
  where
    valA = let factor = (b eq1 / b eq2) in
        (prize eq1 - prize eq2 * factor) / (a eq1 - a eq2 * factor)
    valB = let factor = (a eq1 / a eq2) in
        (prize eq1 - prize eq2 * factor) / (b eq1 - b eq2 * factor)


incPrize :: Equation -> Equation
incPrize m = m { prize = prize m + 10000000000000}


solve :: [Machine] -> Integer
solve = sum . map (\(av, bv) -> 3*av + bv) . mapMaybe solveMachine


machinesP :: Parser [Machine]
machinesP = machineP `sepEndBy1` spaces
  where
    integerP = toInteger <$> intP

    machineP = do
        (xa, ya) <- buttonP "A" <* spaces
        (xb, yb) <- buttonP "B" <* spaces
        xp <- string "Prize:" *> blanksP *> string "X=" *> integerP
        yp <- string "," *> blanksP *> string "Y=" *> integerP
        return (Equation { a = xa % 1, b = xb % 1, prize = xp % 1 },
                Equation { a = ya % 1, b = yb % 1, prize = yp % 1 })

    buttonP b = do
        x <- string "Button" *> blanksP *> string b *> string ":" *> blanksP *> string "X+" *> integerP
        y <- string "," *> blanksP *> string "Y+" *> integerP
        return (x, y)

main :: IO ()
main = applyInput machinesP solve (solve . map (bimap incPrize incPrize))
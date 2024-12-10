module Main where

import AoC                (applyInput)
import Data.Array         (Array, (!), array, bounds, indices, inRange)
import Data.Function      ((&))
import Data.List          (group, sort)
import Text.Parsec        (digit, many1, newline, sepEndBy1)
import Text.Parsec.String (Parser)


type Coord = (Int, Int)
type Tmap = Array Coord Int


n, s, e, w :: Coord -> Coord
n (x, y) = (x - 1, y)
s (x, y) = (x + 1, y)
e (x, y) = (x, y + 1)
w (x, y) = (x, y - 1)


exploreTrail :: ([Coord] -> Int) -> Tmap -> Coord -> Int
exploreTrail score m = score . explore 0
  where
    explore :: Int -> Coord -> [Coord]
    explore val pos
        | m ! pos == 9 = [pos]
        | otherwise =
            let next =
                    map ($ pos) [n, s, e, w]
                    & filter (\c -> inRange (bounds m) c && m ! c == val + 1)
            in concatMap (explore (val + 1)) next


solve :: ([Coord] -> Int) -> Tmap -> Int
solve score m = sum $ map (exploreTrail score m) zeroes
  where
    zeroes = filter ((==0) . (m !)) $ indices m


mapP :: Parser Tmap
mapP = do
    rows <- rowP `sepEndBy1` newline
    let numbered = [((x, y), v) | (x, r) <- zip [0..] rows, (y, v) <- zip [0..] r]
        xSize = maximum $ map (fst . fst) numbered
        ySize = maximum $ map (snd . fst) numbered
    return $ array ((0, 0), (xSize, ySize)) numbered
  where
    rowP = many1 (read . (:[]) <$> digit)


main :: IO ()
main = applyInput mapP (solve (length . group . sort)) (solve length)
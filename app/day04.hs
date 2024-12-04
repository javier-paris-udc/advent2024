module Main where

import AoC                (applyInput)
import Data.Array         (Array, (!), bounds, indices, inRange, listArray)
import Data.Function      ((&))
import Text.Parsec        (many1, newline, oneOf, sepEndBy1)
import Text.Parsec.String (Parser)

type Cross = Array (Int, Int) Char


solveP2 :: Cross -> Int
solveP2 c =
    map (map (map (c !))) allWords
    & filter (all (\w -> w == "MAS" || w == "SAM"))
    & length
  where
    idxs = filter ((=='A') . (c !)) $ indices c

    idxWords (x,y) =
        [[(x-1, y-1), (x, y), (x+1, y+1)]
        ,[(x+1, y-1), (x, y), (x-1, y+1)]]

    allWords =
        map idxWords idxs
        & filter (all (all (inRange (bounds c))))


solveP1 :: Cross -> Int
solveP1 c =
    map (map (c !)) allWords
    & filter (== "MAS")
    & length
  where
    idxs = filter ((=='X') . (c !)) $ indices c

    idxWords (x,y) = filter (all (inRange (bounds c)))
        [(, y) <$> [x+1 .. x+3]
        ,(, y) <$> [x-1, x-2, x-3]
        ,(x, ) <$> [y+1 .. y+3]
        ,(x, ) <$> [y-1, y-2, y-3]
        ,(\n -> (x+n, y+n)) <$> [1..3]
        ,(\n -> (x-n, y-n)) <$> [1..3]
        ,(\n -> (x+n, y-n)) <$> [1..3]
        ,(\n -> (x-n, y+n)) <$> [1..3]
        ]

    allWords = concatMap idxWords idxs


wordSearchP :: Parser Cross
wordSearchP = do
    rows <- many1 (oneOf "XMAS") `sepEndBy1` newline
    let nrows = length rows
        ncols = case rows of
            []    -> undefined  -- cannot happen because the list comes from many1
            col:_ -> length col
    return $ listArray ((0, 0), (nrows - 1, ncols - 1)) (concat rows)


main :: IO ()
main = applyInput wordSearchP solveP1 solveP2
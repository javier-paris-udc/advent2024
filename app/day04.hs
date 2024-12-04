module Main where

import AoC                (applyInput)
import Data.Array         (Array, (!), bounds, indices, inRange, listArray)
import Data.Function      ((&))
import Data.Functor       (($>))
import Data.List          (uncons)
import Text.Parsec        (char, choice, many1, newline, sepEndBy1)
import Text.Parsec.String (Parser)

data Letter = X | M | A | S deriving (Show, Eq)

type Cross = Array (Int, Int) Letter


solveP2 :: Cross -> Int
solveP2 c =
    map (map (map (c !))) allWords
    & filter (all (\w -> w == [M,A,S] || w == [S,A,M]))
    & length
  where
    idxs = filter ((==A) . (c !)) $ indices c

    idxWords (x,y) =
        [[(x-1, y-1), (x, y), (x+1, y+1)]
        ,[(x+1, y-1), (x, y), (x-1, y+1)]]

    allWords =
        map idxWords idxs
        & filter (all (all (inRange (bounds c))))


solveP1 :: Cross -> Int
solveP1 c =
    map (map (c !)) allWords
    & filter (== [X,M,A,S])
    & length
  where
    idxWords (x,y) = filter (all (inRange (bounds c)))
        [(, y) <$> [x .. x+3]
        ,(, y) <$> [x, x-1 .. x-3]
        ,(x, ) <$> [y .. y+3]
        ,(x, ) <$> [y, y-1 .. y-3]
        ,(\n -> (x+n, y+n)) <$> [0..3]
        ,(\n -> (x-n, y-n)) <$> [0..3]
        ,(\n -> (x+n, y-n)) <$> [0..3]
        ,(\n -> (x-n, y+n)) <$> [0..3]
        ]

    allWords = concatMap idxWords (indices c)


wordSearchP :: Parser Cross
wordSearchP = do
    rows <- many1 letterP `sepEndBy1` newline
    let nrows = length rows
        ncols = case uncons rows of
            Nothing -> undefined  -- cannot happen because the list comes from many1
            Just (col, _) -> length col
    return $ listArray ((0, 0), (nrows - 1, ncols - 1)) (concat rows)
  where
    letterP = choice [char 'X' $> X, char 'M' $> M, char 'A' $> A, char 'S' $> S]


main :: IO ()
main = applyInput wordSearchP solveP1 solveP2
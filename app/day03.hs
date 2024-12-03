module Main where

import AoC                (applyInput1, intP)
import Text.Parsec        (anyChar, choice, eof, string, try)
import Text.Parsec.String (Parser)


solve :: [(Int, Int)] -> Int
solve = sum . map (uncurry (*))


mulP :: Parser (Int, Int)
mulP = do
    n1 <- string "mul(" *> intP
    n2 <- string "," *> intP <* string ")"
    return (n1, n2)


mulsP :: Parser [(Int, Int)]
mulsP =
    choice [try mulP >>= (\p -> fmap (p:) mulsP)
           ,anyChar *> mulsP
           ,eof *> pure []
           ]


mulsP2 :: Parser [(Int, Int)]
mulsP2 =
    choice [try mulP >>= (\p -> (p:) <$> mulsP2)
           ,try (string "don't()") *> discardP
           ,anyChar *> mulsP2
           ,eof *> pure []
           ]
  where
    discardP =
        choice [try (string "do()") *> mulsP2
               ,anyChar *> discardP
               ,eof *> pure []
               ]


main :: IO ()
main = do
    applyInput1 mulsP solve
    applyInput1 mulsP2 solve
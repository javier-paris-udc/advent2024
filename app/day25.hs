module Main where

import AoC                (applyInputSWith)
import Data.Either        (lefts, rights)
import Data.Function      ((&))
import Data.List          (transpose)
import Text.Parsec        (many1, newline, oneOf, sepEndBy1, spaces)
import Text.Parsec.String (Parser)


solve :: ([[Int]], [[Int]]) -> IO ()
solve (locks, keys) =
      liftA2 (,) locks keys
    & filter (\(k, l) -> all ((<6) . uncurry (+)) $ zip k l)
    & length
    & print


lockKeyP :: Parser ([[Int]], [[Int]])
lockKeyP = do
    locksAndKeys <- blockP `sepEndBy1` spaces
    return (lefts locksAndKeys, rights locksAndKeys)
  where
    blockP = do
        cols <- transpose <$> (many1 (oneOf "#.") `sepEndBy1` newline)
        return $ getType cols $ map ((subtract 1). length . filter (=='#')) cols

    getType (('#':_):_) = Left
    getType _           = Right


main :: IO ()
main = applyInputSWith lockKeyP () solve
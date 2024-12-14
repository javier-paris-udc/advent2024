module Main where

import           AoC                (applyInput, commaSepP, intP)
import           Control.Arrow      ((>>>))
import           Data.Foldable      (find)
import           Data.Function      (on)
import           Data.List          (group, groupBy, sort)
import           Data.Maybe         (fromMaybe, mapMaybe)
import           Text.Parsec        (sepEndBy1, spaces, string)
import           Text.Parsec.String (Parser)

type Coord = (Int, Int)
data Robot = Robot { pos :: Coord, speed :: Coord } deriving (Show, Eq)


sizeX, sizeY :: Int
sizeX = 101
sizeY = 103


posAfter :: Int -> Robot -> Coord
posAfter n robot = ((posX + vX * n) `mod` sizeX, (posY + vY * n) `mod` sizeY)
  where
    (posX, posY) = pos robot
    (vX, vY) = speed robot


toQuadrant :: Coord -> Maybe Int
toQuadrant (x, y)
    | x < sizeX `div` 2 && y < sizeY `div` 2 = Just 1
    | x < sizeX `div` 2 && y > sizeY `div` 2 = Just 2
    | x > sizeX `div` 2 && y < sizeY `div` 2 = Just 3
    | x > sizeX `div` 2 && y > sizeY `div` 2 = Just 4
    | otherwise        = Nothing


hasTree :: [Coord] -> Bool
hasTree = sort
      >>> groupBy ((==) `on` fst)
      >>> any (any (>=10) . consecutives . map snd)
  where
    consecutives :: [Int] -> [Int]
    consecutives [] = []
    consecutives (a:as)  =
        let (consec, rest) = findBreak a 0 as in consec : consecutives rest

    findBreak _ acc [] = (acc + 1, [])
    findBreak a acc (a1:as)
        | abs (a - a1) <= 1 = findBreak a1 (acc + 1) as
        | otherwise = (acc + 1, as)


solveP2 :: [Robot] -> Int
solveP2 robots = fromMaybe undefined $ find (hasTree . moveRobots) [0..]
  where
    moveRobots n = map (posAfter n) robots


solveP1 :: [Robot] -> Int
solveP1 = product . map length . group . sort . mapMaybe (toQuadrant . posAfter 100)


robotsP :: Parser [Robot]
robotsP = robotP `sepEndBy1` spaces
  where
    robotP = do
        (posX, posY) <- liftA2 (,) (string "p=" *> intP) (commaSepP *> intP <* spaces)
        (sX, sY) <- liftA2 (,) (string "v=" *> intP) (commaSepP *> intP)
        return $ Robot { pos = (posX, posY), speed = (sX, sY) }


main :: IO ()
main = applyInput robotsP solveP1 solveP2

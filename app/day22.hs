module Main where

import           AoC             (applyInput, intP)
import           Control.Arrow   ((>>>))
import           Data.Bits       (xor)
import           Data.Function   ((&))
import           Data.List       (unfoldr)
import qualified Data.Map.Strict as M
import           Text.Parsec     (sepEndBy1, spaces)


rolling4 :: [Int] -> [[Int]]
rolling4 (x1:x2:x3:x4:xs) = [x1,x2,x3,x4] : rolling4 (x2:x3:x4:xs)
rolling4 _ = []


nextSecret :: Int -> Int
nextSecret = mix <*> (* 64)     >>> prune
         >>> mix <*> (`div` 32) >>> prune
         >>> mix <*> (* 2048)   >>> prune
  where
    mix = xor
    prune = (`mod` 16777216)


applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ !x = x
applyN n f !x = applyN (n - 1) f (f x)


solveP2 :: [Int] -> Int
solveP2 = map (secretList >>> map (`mod` 10) >>> sequences)
      >>> foldl1 (M.unionWith (+))
      >>> maximum
  where
    secretList = unfoldr (\x -> Just (x, nextSecret x))
             >>> take 2001

    changeList = zipWith subtract <*> drop 1

    sequences secrets = zip (rolling4 (changeList secrets)) (drop 4 secrets)
                      & foldl' (\m (chSeq, price) -> M.insertWith (\_ x -> x) chSeq price m) M.empty


solveP1 :: [Int] -> Int
solveP1 = sum . map (applyN 2000 nextSecret)


main :: IO ()
main = applyInput (intP `sepEndBy1` spaces) solveP1 solveP2
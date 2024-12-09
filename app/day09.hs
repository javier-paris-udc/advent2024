module Main where

import           AoC           (applyInput)
import           Data.Sequence (Seq, Seq(Empty, (:<|), (:|>)))
import qualified Data.Sequence as Seq
import           Text.Parsec   (digit, many1)


data Extent = Extent { len :: Int, fileId :: Maybe Int } deriving (Show, Eq)


showExtents :: Seq Extent -> String
showExtents Empty = ""
showExtents (Extent { len = eLen, fileId = i } :<| rest) = case i of
    Nothing -> replicate eLen '.'  ++ showExtents rest
    Just n  -> concat (replicate eLen (show n)) ++ showExtents rest


checkSum :: Seq Extent -> Int
checkSum = doSum 0
  where
    doSum _ Empty =
        0
    doSum n (Extent { len = eLen, fileId = Nothing } :<| rest) =
        doSum (n+eLen) rest
    doSum n (Extent { len = eLen, fileId = Just i} :<| rest) =
        i * sum [n .. n + eLen - 1] + doSum (n + eLen) rest


insert :: Extent -> Seq Extent -> Maybe (Seq Extent)
insert eIns@(Extent { len = eLen }) s = case s of
    Empty ->
        Nothing
    e@Extent { fileId = Just _ }:<| rest ->
        (e :<|) <$> insert eIns rest
    e@Extent { len = eLenFree } :<| rest
        | eLenFree > eLen -> Just $ eIns :<| e { len = eLenFree - eLen } :<| rest
        | eLenFree == eLen -> Just $ eIns :<| rest
        | otherwise -> (e :<|) <$> insert eIns rest


compactP2 :: Seq Extent -> Seq Extent
compactP2 s = case s of
    Empty ->
        Empty
    e@Extent { fileId = Just _}:<| rest ->
        e :<| compactP2 rest
    rest :|> e@Extent {fileId = Nothing } ->
        compactP2 rest :|> e
    rest :|> e ->
        case insert e rest of
            Nothing -> compactP2 rest :|> e
            Just nRest -> compactP2 nRest :|> e { fileId = Nothing }


compactP1 :: Seq Extent -> Seq Extent
compactP1 s = case s of
    Empty ->
        Empty
    e@Extent { fileId = Just _ } :<| rest ->
        e :<| compactP1 rest
    rest :|> e@Extent {len = _, fileId = Nothing} ->
        compactP1 rest :|> e
    le@(Extent { len = _, fileId = Nothing } :<| Empty) ->
        le
    ef@Extent { len = eLenF } :<| (rest :|> eb@Extent { len = eLenB })
        | eLenF > eLenB ->
            eb :<| (compactP1 (ef { len = eLenF - eLenB } :<| rest) :|> eb { fileId = Nothing })
        | eLenF == eLenB ->
            eb :<| (compactP1 rest :|> ef)
        | otherwise ->
            eb { len = eLenF } :<| compactP1 (rest :|> eb { len = eLenB - eLenF })


toDiskSeq :: [Int] -> Seq Extent
toDiskSeq = Seq.fromList . filter ((/=0) . len) . toExtents [0..]
  where
    toExtents (i:_) [n] = [Extent { len = n, fileId = Just i }]
    toExtents (i:ids) (nFile:nFree:rest) =
         Extent { len = nFile, fileId = Just i }
        :Extent { len = nFree, fileId = Nothing }
        :toExtents ids rest
    toExtents _ _ = []


solveP2 :: [Int] -> Int
solveP2 = checkSum . compactP2 . toDiskSeq


solveP1 :: [Int] -> Int
solveP1 = checkSum . compactP1 . toDiskSeq


main :: IO ()
main = applyInput (many1 (read . (:[]) <$> digit)) solveP1 solveP2
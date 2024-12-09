module Main where

import           AoC           (applyInput)
import           Data.Sequence (Seq, Seq(Empty, (:<|), (:|>)))
import qualified Data.Sequence as Seq
import           Text.Parsec   (digit, many1)
import qualified Data.IntMap.Strict as Map
import Data.List (sortOn, (\\), sortBy)
import qualified Data.IntSet as Set
import Data.Maybe (isJust)
import Data.Function (on)


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


defragP2 :: Seq Extent -> Seq Extent
defragP2 s = case s of
    Empty ->
        Empty
    e@Extent { fileId = Just _}:<| rest ->
        e :<| defragP2 rest
    rest :|> e@Extent {fileId = Nothing } ->
        defragP2 rest :|> e
    rest :|> e ->
        case insert e rest of
            Nothing -> defragP2 rest :|> e
            Just nRest -> defragP2 nRest :|> e { fileId = Nothing }


compressP2 :: [Extent] -> Seq Extent
compressP2 l = doCompress Empty Set.empty l prioL
  where
    prioL = Map.map (sortBy (flip (compare `on` fileId))) $ foldr insertAll Map.empty (filter (isJust . fileId) l)
    insertAll e m = foldr (\sz mAcc -> Map.insertWith (++) sz [e] mAcc) m [len e .. 9]

    doCompress s done eList pending =
        case eList of
            [] -> s
            e@Extent { fileId = Just i } : rest
                | i `Set.member` done -> doCompress s done (e { fileId = Nothing } : rest) pending
                | otherwise -> doCompress (s :|> e) done rest (del e pending)
            e@Extent { len = eLen, fileId = Nothing } : rest ->
                case pending Map.! eLen of
                    [] -> doCompress (s :|> e) done rest pending
                    e2@Extent { len = eLen2, fileId = Just i } : _
                        | eLen2 < eLen ->
                            doCompress
                                (s :|> e2)
                                (Set.insert i done)
                                (e { len = eLen - eLen2 } : rest)
                                (del2 e2 pending)
                        | otherwise ->
                            doCompress
                                (s :|> e2)
                                (Set.insert i done)
                                rest
                                (del2 e2 pending)
                    _ -> undefined

del e pending = foldr (\extLen m -> Map.adjust (\\ [e]) extLen m) pending [len e .. 9]

del2 e pending = foldr (\extLen m -> Map.adjust (\\ [e]) extLen m) pending [len e .. 9]


defragP1 :: Seq Extent -> Seq Extent
defragP1 s = case s of
    Empty ->
        Empty
    e@Extent { fileId = Just _ } :<| rest ->
        e :<| defragP1 rest
    rest :|> e@Extent {len = _, fileId = Nothing} ->
        defragP1 rest :|> e
    le@(Extent { len = _, fileId = Nothing } :<| Empty) ->
        le
    ef@Extent { len = eLenF } :<| (rest :|> eb@Extent { len = eLenB })
        | eLenF > eLenB ->
            eb :<| (defragP1 (ef { len = eLenF - eLenB } :<| rest) :|> eb { fileId = Nothing })
        | eLenF == eLenB ->
            eb :<| (defragP1 rest :|> ef)
        | otherwise ->
            eb { len = eLenF } :<| defragP1 (rest :|> eb { len = eLenB - eLenF })


toDiskSeq :: [Int] -> Seq Extent
toDiskSeq = Seq.fromList . filter ((/=0) . len) . toExtents [0..]
--  where
toExtents (i:_) [n] = [Extent { len = n, fileId = Just i }]
toExtents (i:ids) (nFile:nFree:rest) =
        Extent { len = nFile, fileId = Just i }
    :Extent { len = nFree, fileId = Nothing }
    :toExtents ids rest
toExtents _ _ = []


solveP2 :: [Int] -> Int
solveP2 = --checkSum . defragP2 . toDiskSeq
  checkSum . compressP2 . filter ((/=0) . len) . toExtents [0 .. ]

solveP1 :: [Int] -> Int
solveP1 = checkSum . defragP1 . toDiskSeq


main :: IO ()
main = applyInput (many1 (read . (:[]) <$> digit)) solveP1 solveP2
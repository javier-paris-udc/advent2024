module Main where

import           AoC                (applyInput)
import           Data.Foldable      (toList)
import           Data.Function      ((&), on)
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet        as Set
import           Data.List          (sortBy)
import           Data.Maybe         (fromJust, isJust)
import           Data.Sequence      (Seq, Seq(Empty, (:<|), (:|>)))
import qualified Data.Sequence      as Seq
import           Text.Parsec        (digit, many1)


data Extent = Extent { len :: Int, fileId :: Maybe Int } deriving (Show, Eq)


isFile, isEmpty :: Extent -> Bool
isFile = isJust . fileId
isEmpty = not . isFile


showExtents :: Foldable f => f Extent -> String
showExtents = foldr (\e rest -> showExtent e ++ rest ) ""
  where
    showExtent (Extent { len = eLen, fileId = i }) = case i of
        Nothing -> replicate eLen '.'
        Just n  -> concat (replicate eLen (show n))


checkSum :: [Extent] -> Int
checkSum = doSum 0
  where
    doSum _ [] =
        0
    doSum n (Extent { len = eLen, fileId = Nothing } : rest) =
        doSum (n+eLen) rest
    doSum n (Extent { len = eLen, fileId = Just i} : rest) =
        i * sum [n .. n + eLen - 1] + doSum (n + eLen) rest


defragP2 :: [Extent] -> [Extent]
defragP2 l = doDefrag Set.empty l prioL
  where
    prioL = filter isFile l
          & foldr insertInLengths Map.empty
          & Map.map (sortBy (flip compare `on` fileId))

    insertInLengths e m = foldr (\sz mAcc -> Map.insertWith (++) sz [e] mAcc) m [len e .. 9]

    doDefrag done eList pending =
        case eList of
            [] -> []
            e@Extent { fileId = Just i } : rest
                | i `Set.member` done ->
                    doDefrag done (e { fileId = Nothing } : rest) pending
                | otherwise ->
                    e : doDefrag (Set.insert i done) rest pending
            e@Extent { len = eLen, fileId = Nothing } : rest ->
                case dropWhile ((`Set.member` done) . fromJust . fileId) (pending Map.! eLen) of
                    [] ->
                        e : doDefrag done rest (Map.insert eLen [] pending)
                    e2@Extent { len = eLen2, fileId = Just i } : eLenPending ->
                        let newEList =
                                if eLen2 < eLen
                                then e { len = eLen - eLen2 } : rest
                                else rest
                        in e2 : doDefrag (Set.insert i done) newEList (Map.insert eLen eLenPending pending)
                    _ -> undefined


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


toDiskSeq :: [Int] -> [Extent]
toDiskSeq = filter ((/=0) . len) . toExtents [0..]
  where
    toExtents (i:_) [n] = [Extent { len = n, fileId = Just i }]
    toExtents (i:ids) (nFile:nFree:rest) =
        Extent { len = nFile, fileId = Just i }
        :Extent { len = nFree, fileId = Nothing }
        :toExtents ids rest
    toExtents _ _ = []


solveP2 :: [Int] -> Int
solveP2 = checkSum . defragP2 . toDiskSeq

solveP1 :: [Int] -> Int
solveP1 = checkSum . toList . defragP1 . Seq.fromList . toDiskSeq


main :: IO ()
main = applyInput (many1 (read . (:[]) <$> digit)) solveP1 solveP2
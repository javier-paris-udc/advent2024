module Main where

import           AoC                              (applyInputSWith, intP)
import           Control.Monad.Trans.State.Strict (State, evalState, gets, modify')
import           Data.Bits                        ((.&.))
import           Data.Function                    ((&))
import           Data.Functor                     (($>))
import           Data.List                        (intercalate, sort)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (listToMaybe)
import           Text.Parsec                      (alphaNum, choice, many1, newline, sepEndBy1, spaces, string)
import           Text.Parsec.String               (Parser)
import           Test.QuickCheck                  (Args (chatty)
                                                  ,arbitrary
                                                  ,forAll
                                                  ,isSuccess
                                                  ,quickCheckWithResult
                                                  ,stdArgs
                                                  ,vectorOf
                                                  ,within)


data Op = And | Or | Xor deriving (Show, Eq)
data Gate = Val Bool | Expr Op String String deriving (Show, Eq)
type Circuit = Map.Map String Gate


opFun :: Op -> Bool -> Bool -> Bool
opFun And = (&&)
opFun Or  = (||)
opFun Xor = (/=)


eval :: String -> State Circuit Bool
eval var = do
    varVal <- gets (Map.! var)
    case varVal of
        Val val -> pure val
        Expr op var1 var2 -> do
            val1 <- eval var1
            val2 <- eval var2

            let val = opFun op val1 val2
            modify' (Map.insert var (Val val))

            pure val


toInt :: [Bool] -> Int
toInt = foldr (\v acc -> if v then acc * 2 + 1 else acc * 2) 0


findDependentVars :: String -> Circuit -> [String]
findDependentVars var circuit = case circuit Map.! var of
    Val _ -> []
    Expr _ var1 var2 -> var : findDependentVars var1 circuit ++ findDependentVars var2 circuit


switch :: Circuit -> (String, String) -> ((String,String), Circuit)
switch circuit (v1, v2) =
    let gate1 = circuit Map.! v1
        gate2 = circuit Map.! v2
    in ((v1, v2), Map.insert v2 gate1 $ Map.insert v1 gate2 circuit)


check :: Circuit -> IO [(String,String)]
check circuit0 = doCheck circuit0 1
  where
    doCheck circuit bit = do
        if bit > maxBit then pure []
        else do
            bitIsFine <- isCorrect bit circuit
            if bitIsFine then doCheck circuit (bit + 1)
            else correct bit circuit

    isCorrect bit circuit =
        isSuccess <$> quickCheckWithResult
                        (stdArgs { chatty = False })
                        (forAll (genNums bit) (\ls -> within 1000 $ sumBitsProp circuit bit ls))

    correct bit circuit =
        let badVar       = zVars !! (bit - 1)
            dependents   = findDependentVars badVar circuit
            switchable   = varsSuchThat (`notElem` "xy") circuit
            switchedVars = [switch circuit (v1, v2) | v1 <- dependents, v2 <- switchable, v1 /= v2]
                           & foldr (\c@(_, cir) m -> do
                                cor <- isCorrect bit cir
                                if cor then pure c
                                else m
                                ) undefined
        in do
            (vars, sol) <- switchedVars
            (vars :) <$> doCheck sol (bit + 1)


    genNums bits = liftA2 (,) (vectorOf bits arbitrary) (vectorOf bits arbitrary)

    sumBitsProp circuit bit (l1, l2) =
        addF circuit bit l1 l2 == (toInt l1 + toInt l2) .&. (2^bit - 1)

    xVars = varsSuchThat (== 'x') circuit0
    yVars = varsSuchThat (== 'y') circuit0
    zVars = varsSuchThat (== 'z') circuit0

    maxBit = length zVars - 1

    addF = add xVars yVars zVars



add :: [String] -> [String] -> [String] -> Circuit -> Int -> [Bool] -> [Bool] -> Int
add xVars yVars zVars circuit bits xVals yVals =
      foldl' (\m (x, v) -> Map.insert x (Val v) m) circuit (zip xVars xVals)
    & (\cir -> foldl' (\m (y, v) -> Map.insert y (Val v) m) cir (zip yVars yVals))
    & evalState (traverse eval (take bits zVars))
    & toInt


varsSuchThat :: (Char -> Bool) -> Circuit -> [String]
varsSuchThat that circuit = sort $ filter (maybe False that . listToMaybe) $ Map.keys circuit


solveP2 :: Circuit -> IO [String]
solveP2 circuit = (sort . concat . map pairToList) <$> check circuit
  where
    pairToList (a, b) = [a, b]


solveP1 :: Circuit -> Int
solveP1 circuit0 = toInt zVals
  where
    zVars = varsSuchThat (=='z') circuit0
    zVals = evalState (traverse eval zVars) circuit0


circuitP :: Parser Circuit
circuitP = liftA2 (++) (valP `sepEndBy1` newline) (spaces >> gateP `sepEndBy1` spaces)
         & fmap Map.fromList
  where
    valP = do
        name <- idP <* string ":" <* spaces
        val  <- intP
        return $ (name, Val (val == 1))

    gateP = do
        name1 <- idP <* spaces
        op    <- opP <* spaces
        name2 <- idP <* spaces <* string "->" <* spaces
        res   <- idP
        return $ (res, Expr op name1 name2)

    idP = many1 alphaNum

    opP = choice [string "AND" $> And
                 ,string "OR"  $> Or
                 ,string "XOR" $> Xor
                 ]


main :: IO ()
main = applyInputSWith circuitP () solve
  where
    solve circuit = do
        print $ solveP1 circuit
        res <- solveP2 circuit
        putStrLn $ intercalate "," res
module Main where

import           AoC                       (applyInputSWith, commaSepP, intP)
import           Control.Monad.Trans.State (State, execState, gets, modify')
import           Data.Bits                 (xor)
import qualified Data.IntMap.Strict        as Map
import           Data.List                 (intercalate)
import           Data.Maybe                (fromJust)
import           Text.Parsec               (sepEndBy1, spaces, string)
import           Text.Parsec.String        (Parser)


data Computer = Computer { a :: Int, b :: Int, c :: Int, program :: Map.IntMap Int, output :: [Int], ip :: Int }
    deriving (Show, Eq)

type Instruction = Int -> State Computer ()

opCodes :: Map.IntMap Instruction
opCodes = Map.fromList
    [(0, adv)
    ,(1, bxl)
    ,(2, bst)
    ,(3, jnz)
    ,(4, bxc)
    ,(5, out)
    ,(6, bdv)
    ,(7, cdv)
    ]


combo :: Int -> State Computer Int
combo operand
    | operand <= 3 = pure operand
    | operand == 4 = gets a
    | operand == 5 = gets b
    | operand == 6 = gets c
    | otherwise    = undefined

cdv :: Instruction
cdv operand = do
    arg <- combo operand
    modify' (\comp -> comp { c = a comp `div` 2^arg, ip = ip comp + 2 })


bdv :: Instruction
bdv operand = do
    arg <- combo operand
    modify' (\comp -> comp { b = a comp `div` 2^arg, ip = ip comp + 2 })


out :: Instruction
out operand = do
    arg <- combo operand
    modify' (\comp -> comp { output = arg `mod` 8 : output comp, ip = ip comp + 2 })


bxc :: Instruction
bxc _ = modify' (\comp -> comp { b = b comp `xor` c comp, ip = ip comp + 2 })


jnz :: Instruction
jnz operand = do
    aVal <- gets a
    if aVal == 0 then modify' (\comp -> comp { ip = ip comp + 2 })
    else modify' (\comp -> comp { ip = operand })


bst :: Instruction
bst operand = do
    arg <- combo operand
    modify' (\comp -> comp { b = arg `mod` 8, ip = ip comp + 2 })


bxl :: Instruction
bxl operand = modify' (\comp -> comp { b = b comp `xor` operand, ip = ip comp + 2 })


adv :: Instruction
adv operand = do
    arg <- combo operand
    modify' (\comp -> comp { a = a comp `div` 2^arg, ip = ip comp + 2 })


evalProg :: State Computer ()
evalProg = do
    inst <- gets (\comp -> program comp Map.!? ip comp)
    case inst of
        Nothing -> pure ()
        Just i -> do
            operand <- gets (\comp -> program comp Map.! (ip comp + 1))
            opCodes Map.! i $ operand
            evalProg

-- This is ad-hoc for my input, it would probably not work on a different one
solveP2 :: Computer -> Int
solveP2 comp = fromJust $ findProg 0 prog
  where
    prog = reverse $ map snd $ Map.toList $ program comp

    findProg aVal [] = Just aVal
    findProg aVal (x:xs) =
        check aVal xs $ filter (testComp x aVal) [0..7]

    check _ _ [] = Nothing
    check aVal xs (v:vs) = case findProg (aVal * 8 + v) xs of
        Nothing -> check aVal xs vs
        sol -> sol

    testComp x aVal i = case solveP1 (comp { a = aVal*8 + i }) of
        [] -> undefined
        y:_ -> x == y


solveP1 :: Computer -> [Int]
solveP1 = reverse . output . execState evalProg


computerP :: Parser Computer
computerP = do
    a0 <- regP "A"
    b0 <- regP "B"
    c0 <- regP "C"
    program0 <- spaces *> string "Program:" *> spaces *> intP `sepEndBy1` commaSepP

    return $ Computer {a = a0
                      ,b = b0
                      ,c = c0
                      ,program = Map.fromList (zip [0..] program0)
                      ,output = []
                      ,ip = 0
                      }
  where
    regP reg = string "Register" *> spaces *> string reg *> string ":" *> spaces *> intP <* spaces


main :: IO ()
main = applyInputSWith computerP () solveAndPrint
  where
    solveAndPrint computer = do
        putStrLn $ intercalate "," $ map show $ solveP1 computer
        print $ solveP2 computer
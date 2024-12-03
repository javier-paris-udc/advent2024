module AoC where

import Data.Bifunctor     (first)
import Data.List          (unfoldr)
import Text.Parsec        (char
                          ,many1
                          ,digit
                          ,option
                          ,oneOf
                          ,Parsec
                          ,runParser
                          ,spaces
                          ,string
                          ,try)
import Text.Parsec.String (Parser)
import System.Environment (getArgs, getProgName)
import Control.Monad      (void)


groupsOf :: Int -> [a] -> [[a]]
groupsOf n = unfoldr (\l -> if null l then Nothing else Just (splitAt n l))


fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a


snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b


thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c



intP :: Parsec String a Int
intP =
    do
        sign <- option 1 (char '-' >> return (-1))
        num  <- read <$> many1 digit
        return (num * sign)


blankP :: Parsec String a ()
blankP = void $ oneOf " \t"


blanksP :: Parsec String a ()
blanksP = void $ many1 blankP


sep :: String -> Parsec String a ()
sep s = try $ spaces >> string s >> spaces


commaSepP :: Parsec String a ()
commaSepP = sep ","


getParsedInput :: Parsec String s a -> s -> String -> IO (Maybe a)
getParsedInput parser state file = do
    fileContents <- readFile file
    case runParser parser state file fileContents of
        Right res -> return $ Just res
        Left  _   -> return Nothing


parseFromArg :: Parsec String s a -> s -> IO (Either String a)
parseFromArg parser state = do
    args <- getArgs
    prog <- getProgName
    case args of
        [inputFile] ->
            do
                fileContent <- readFile inputFile
                return $ first show $ runParser parser state inputFile fileContent
        _ ->
            return $ Left $ "Use: "++prog++" input"


applyInputSWith :: Parsec String s a -> s -> (a -> IO ()) -> IO ()
applyInputSWith parser state f =
    do
        parseRes <- parseFromArg parser state
        case parseRes of
            Left err ->
                putStrLn err
            Right parsedRes ->
                f parsedRes


applyInput1S :: (Show b) => Parsec String s a -> s -> (a -> b) -> IO ()
applyInput1S parser state solve = applyInputSWith parser state (print . solve)


applyInput1 :: (Show b) => Parser a -> (a -> b) -> IO ()
applyInput1 parser solve = applyInputSWith parser () (print . solve)


applyInput :: (Show b, Show c) => Parser a -> (a -> b) -> (a -> c) -> IO ()
applyInput = flip applyInputS ()


applyInputS :: (Show b, Show c) => Parsec String s a -> s -> (a -> b) -> (a -> c) -> IO ()
applyInputS parser state solveP1 solveP2 =
    applyInputSWith parser state solveAndPrint
  where
    solveAndPrint input = do
        print $ solveP1 input
        print $ solveP2 input

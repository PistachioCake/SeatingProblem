module Main where

import Lib
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Control.Monad (forever)

main :: IO ()
main = do
    putStrLn "Table Size? > "
    tableSize <- read <$> getLine
    putStrLn "Class Size? > "
    classSize <- read <$> getLine
    putStrLn "Num Classes? > "
    numClasses <- read <$> getLine
    -- let numClasses = (tableSize * classSize - 1) `div` (tableSize - 1)
    let sol = {- maybe [] id $ -} solve tableSize classSize numClasses [] [] []
    -- print (length sol)
    putStrLn (showSolution $ head sol)
    forever $ do
        a <- getLine
        if a == "q" then exitSuccess else return ()
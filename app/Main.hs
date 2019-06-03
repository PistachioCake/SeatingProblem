module Main where

import Lib

main :: IO ()
main = do
    putStrLn "HI!\n" 
    let sol = maybe [] id $ solve [] [] []
    print $ all (== (head sol)) sol
    print sol
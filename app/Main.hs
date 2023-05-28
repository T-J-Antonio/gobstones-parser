module Main (main) where

import Lib
import Parser
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    let toApply = maybe (error "Parsing error") snd (runParser programP contents)
    putStrLn $ toApply $ newBoard 3
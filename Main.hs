module Main (main) where

import FSInfo (FSInfo(..), fsInfo)

main :: IO ()
main = do
    p <- getLine
    i <- fsInfo p
    putStrLn $ show i


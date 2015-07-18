module MiscUtils (
    flatten, splitElem, dropElem,
    startsWith,
    fsAllowedName,
    Color(..), ColorIntensity(..),
    putColoredStrLn
) where

import Data.List (splitAt)
import System.Console.ANSI
import System.IO (hFlush, stdout)

flatten :: [[a]] -> [a]
flatten = foldl (++) []

splitElem :: Int -> [a] -> ([a], a, [a])
splitElem i x =
    let n = length x
     in if i >= 0 && i < n
            then let (a, b) = splitAt i x
                  in (a, head b, tail b)
            else error $ "Invalid index " ++ show i ++ " for range " ++ show (0, n - 1)

dropElem :: Int -> [a] -> [a]
dropElem i x = let (a, _, b) = splitElem i x in a ++ b

startsWith :: Ord a => [a] -> [a] -> Bool
startsWith x y = let [n, m] = map length [x, y] in m >= n && take n y == x

fsAllowedName :: FilePath -> Bool
fsAllowedName = not . startsWith "."

putColoredStrLn :: (ColorIntensity, Color) -> String -> IO ()
putColoredStrLn (i, c) s = do
    setSGR [SetColor Foreground i c]
    putStrLn s
    setSGR []
    hFlush stdout


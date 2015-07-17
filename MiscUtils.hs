module MiscUtils (
    startsWith,
    fsAllowedName,
    Color(..), ColorIntensity(..),
    putColoredStrLn
) where

import System.Console.ANSI
import System.IO (hFlush, stdout)

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


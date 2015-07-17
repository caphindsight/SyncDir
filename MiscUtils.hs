module MiscUtils (
    startsWith,
    fsAllowedName
) where

startsWith :: Ord a => [a] -> [a] -> Bool
startsWith x y = let [n, m] = map length [x, y] in m >= n && take n y == x

fsAllowedName :: FilePath -> Bool
fsAllowedName = not . startsWith "."


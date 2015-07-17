module Main (main) where

import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))
import MiscUtils
import FSInfo
import FSAction
import FSDiff

configFile :: IO FilePath
configFile = do
    homeDir <- getHomeDirectory
    return $ homeDir </> ".sync_dir_config"

data Config = Config [(FilePath, FilePath)] deriving (Read, Show)

fsSync :: (FilePath, FilePath) -> IO ()
fsSync (s, t) = do
    a <- fsInfo s
    b <- fsInfo t
    let d = fsDiff a b
    putColoredStrLn (Vivid, Magenta) $ "=== " ++ s ++ " ~> " ++ t ++ " ==="
    fsDo (s, t) d

main :: IO ()
main = do
    cfg <- configFile
    cfgExists <- doesFileExist cfg
    if cfgExists
        then do
            Config config <- readFile cfg >>= return . read :: IO Config
            foldl1 (>>) $ map fsSync config
            putColoredStrLn (Vivid, Green) "=== OK, done!"
        else
            putStrLn "Config file ~/.sync_dir_config not found"


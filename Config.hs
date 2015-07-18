{-# LANGUAGE BangPatterns #-}

module Config (
    configFile,
    ConfigEntry(..),
    Config(..),
    readConfig, writeConfig, mutateConfig
) where

import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))
import System.IO (readFile, writeFile)

configFile :: IO FilePath
configFile = do
    homeDir <- getHomeDirectory
    return $ homeDir </> ".sync_dir_config"

data ConfigEntry = ConfigEntry_Sync {
    configEntry_Sync_sourcePath :: !FilePath,
    configEntry_Sync_targetPath :: !FilePath,
    configEntry_Sync_enabled    :: !Bool
} deriving (Show, Read)

data Config = Config {
    config_entries :: ![ConfigEntry]
} deriving (Show, Read)

readConfig :: IO Config
readConfig = do
    cfgFile <- configFile
    cfgFileExists <- doesFileExist cfgFile
    if cfgFileExists
        then readFile cfgFile >>= return . read
        else return $ Config { config_entries = [] }

writeConfig :: Config -> IO ()
writeConfig cfg = do
    cfgFile <- configFile
    writeFile cfgFile $ show cfg

mutateConfig :: (Config -> Config) -> IO ()
mutateConfig f = do
    !cfg <- readConfig
    !newCfg <- return $ f cfg
    writeConfig newCfg


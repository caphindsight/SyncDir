{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import Prelude hiding (drop)
import System.Console.CmdArgs
import System.Directory (removeFile, doesFileExist)
import System.IO (hFlush, stdout)
import MiscUtils
import Config
import FSInfo
import FSAction
import FSDiff

data SyncDir =
    ListConfigEntries |
    
    AddConfigEntry {
        sourceDir  :: String,
        targetDir  :: String,
        disabled   :: Bool
    } |
    
    DropConfigEntry {
        identifier :: Int
    } |

    EnableConfigEntry {
        identifier :: Int
    } |

    DisableConfigEntry {
        identifier :: Int
    } |

    Sync |

    SyncOnce {
        sourceDir :: String,
        targetDir :: String
    } |

    DropAll

    deriving (Show, Data, Typeable)

listConfigEntries = ListConfigEntries &= name "list" &= help "List all entries from the sync-dir configuration file"

addConfigEntry = AddConfigEntry {
           sourceDir = def &= argPos 0 &= typ "SYNC_SOURCE_DIR",
           targetDir = def &= argPos 1 &= typ "SYNC_TARGET_DIR",
           disabled  = def
       } &= name "add" &= help "Add an entry to the sync-dir configuration file"

dropConfigEntry = DropConfigEntry {
           identifier = def &= argPos 0 &= typ "IDENTIFIER"
       } &= name "drop" &= help "Drop an entry from the sync-dir configuration file"

enableConfigEntry = EnableConfigEntry {
           identifier = def &= argPos 0 &= typ "IDENTIFIER"
       } &= name "enable" &= help "Enable an entry in the sync-dir configuration file"

disableConfigEntry = DisableConfigEntry {
           identifier = def &= argPos 0 &= typ "IDENTIFIER"
       } &= name "disable" &= help "Disable an entry in the sync-dir configuration file"

sync = Sync &= name "sync" &= help "Synchronize all enabled entries from the configuration file"

syncOnce = SyncOnce {
               sourceDir = def &= argPos 0 &= typ "SYNC_SOURCE_DIR",
               targetDir = def &= argPos 1 &= typ "SYNC_TARGET_DIR"
           } &= name "sync-once" &= help "Synchronize two given directories"

dropAll = DropAll &= name "drop-all" &= help "Drops all entries from the sync-dir configuration file"

main :: IO ()
main = run =<< cmdArgs (modes [listConfigEntries, addConfigEntry, dropConfigEntry,
                                 enableConfigEntry, disableConfigEntry, sync, syncOnce, dropAll]
                                 &= program "dsync"
                                 &= summary "dsync v1.0; copyright (C) hindsight <hindsight@yandex.ru>"
                                 &= help "Keep pairs of directories synchronized with just one CLI command"
                              )

fsSync :: (FilePath, FilePath) -> IO ()
fsSync (s, t) = do
    a <- fsInfo s
    b <- fsInfo t
    let d = fsDiff a b
    putColoredStrLn (Vivid, Magenta) $ "=== " ++ s ++ " ~> " ++ t ++ " ==="
    fsDo (s, t) d

run :: SyncDir -> IO ()

run ListConfigEntries = do
    cfg <- readConfig
    let listEntry (i, entry) = putStrLn $ (if configEntry_Sync_enabled entry then "" else "# ") ++
                                           (show i) ++ ". " ++ (configEntry_Sync_sourcePath entry) ++
                                           " ~> " ++ (configEntry_Sync_targetPath entry)
    foldl (>>) (return ()) $ map listEntry $ zip [0..] $ config_entries cfg

run (AddConfigEntry { sourceDir = s, targetDir = t, disabled = d }) = do
    let entry = ConfigEntry_Sync { configEntry_Sync_sourcePath = s,
                                   configEntry_Sync_targetPath = t,
                                   configEntry_Sync_enabled = not d }
     in mutateConfig $ \cfg -> Config (config_entries cfg ++ [entry])

run (DropConfigEntry { identifier = i }) = do
    mutateConfig $ \cfg -> Config (dropElem i $ config_entries cfg)

run (EnableConfigEntry { identifier = i }) = do
    mutateConfig $ \cfg -> (let (a, e, b) = splitElem i (config_entries cfg)
                            in Config $ a ++ [e { configEntry_Sync_enabled = True }] ++ b)

run (DisableConfigEntry { identifier = i }) = do
    mutateConfig $ \cfg -> (let (a, e, b) = splitElem i (config_entries cfg)
                            in Config $ a ++ [e { configEntry_Sync_enabled = False }] ++ b)

run Sync = do
    cfg <- readConfig
    foldl (>>) (return ()) $ map (\e -> fsSync (configEntry_Sync_sourcePath e, configEntry_Sync_targetPath e)) $
                        filter configEntry_Sync_enabled $ config_entries cfg
    putColoredStrLn (Vivid, Magenta) "Done!"

run (SyncOnce { sourceDir = s, targetDir = t }) = do
    fsSync (s, t)
    putColoredStrLn (Vivid, Magenta) "Done!"

run DropAll = do
    putStrLn "You are about to drop all entries from your sync-dir configuration file."
    putStr   "Are you absolutely sure? (y/n): "
    hFlush stdout
    confirmation <- getLine
    if confirmation == "y"
        then do
            cfgFile <- configFile
            cfgFileExists <- doesFileExist cfgFile
            if cfgFileExists
                then removeFile cfgFile
                else return ()
            putStrLn "Done."
        else putStrLn "User canceled this dangerous action."


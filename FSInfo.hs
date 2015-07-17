module FSInfo (
    FSInfo(..),
    fsInfo
) where

import System.Directory (getDirectoryContents)
import System.FilePath ((</>), takeFileName)
import MiscUtils (fsAllowedName)
import FSType (FSType(..), fsType)
import FSTime (FSTime, fsTime)

data FSInfo = FSInfo_Dir  String [FSInfo]
            | FSInfo_File String FSTime
            deriving (Show)

fsInfo :: FilePath -> IO FSInfo
fsInfo p = do
    let name = takeFileName p
    t <- fsType p
    case t of
        Just FSType_Dir  -> do
            items <- getDirectoryContents p >>= return . filter fsAllowedName
            infos <- sequence $ map (fsInfo . (p</>)) items
            return $ FSInfo_Dir name infos

        Just FSType_File -> do
            mTime <- fsTime p
            return $ FSInfo_File name mTime

        Nothing -> error $ "Path not found: " ++ p


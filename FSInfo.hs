module FSInfo (
    FSInfo(..),
    fsInfo
) where

import System.Directory (getDirectoryContents)
import System.FilePath ((</>), takeFileName)
import MiscUtils (fsAllowedName)
import FSType (FSType(..), fsType)
import FSTime (FSTime, fsTime)

data FSInfo = FSInfo_Dir [(String, FSInfo)]
            | FSInfo_File FSTime
            deriving (Show)

fsInfo :: FilePath -> IO FSInfo
fsInfo p = do
    let name = takeFileName p
    t <- fsType p
    case t of
        Just FSType_Dir  -> do
            items <- getDirectoryContents p >>= return . filter fsAllowedName

            infoVals <- sequence $ map (fsInfo . (p</>)) items
            infos <- return $ zip items infoVals
            return $ FSInfo_Dir infos

        Just FSType_File -> do
            mTime <- fsTime p
            return $ FSInfo_File mTime

        Nothing -> error $ "Path not found: " ++ p


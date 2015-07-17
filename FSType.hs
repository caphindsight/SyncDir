module FSType (
    FSType(..),
    fsType
) where

import System.Directory (doesDirectoryExist, doesFileExist)

data FSType = FSType_Dir
            | FSType_File
            deriving (Eq)

fsType :: FilePath -> IO (Maybe FSType)
fsType p = do
    d <- doesDirectoryExist p
    f <- doesFileExist p
    return $ if d || f
                then
                    if d
                        then Just FSType_Dir
                        else Just FSType_File
                else Nothing


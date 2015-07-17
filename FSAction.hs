module FSAction (
    FSAction(..), FSActionType(..),
    putFSAction, performFSAction, fsDo
) where

import System.Directory (copyFile, removeFile, createDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import MiscUtils

data FSActionType = FSActionType_AddFile
                  | FSActionType_ModifyFile
                  | FSActionType_DeleteFile
                  | FSActionType_SkipFile
                  | FSActionType_AddDir
                  | FSActionType_DeleteDir
                  | FSActionType_EnterDir
                  | FSActionType_LeaveDir

data FSAction = FSAction FSActionType FilePath

putFSAction :: FSAction -> IO ()
putFSAction (FSAction ty f) =
    case ty of
        FSActionType_AddFile    -> putColoredStrLn (Dull, Green)  $ " +  " ++ f
        FSActionType_ModifyFile -> putColoredStrLn (Dull, Yellow) $ " *  " ++ f
        FSActionType_DeleteFile -> putColoredStrLn (Dull, Red)    $ " -  " ++ f
        FSActionType_SkipFile   -> putStrLn                       $ "    " ++ f
        FSActionType_AddDir     -> putColoredStrLn (Vivid, Green) $ "[+] " ++ f
        FSActionType_DeleteDir  -> putColoredStrLn (Vivid, Red)   $ "[-] " ++ f
        FSActionType_EnterDir   -> putColoredStrLn (Vivid, White) $ ">>> " ++ f
        FSActionType_LeaveDir   -> putColoredStrLn (Vivid, White) $ "<<< " ++ f

performFSAction :: (FilePath, FilePath) -> FSAction -> IO ()
performFSAction (sourceRoot, targetRoot) (FSAction ty path) =
    let s = sourceRoot </> path;
        t = targetRoot </> path
     in case ty of
            FSActionType_AddFile    -> copyFile s t
            FSActionType_ModifyFile -> copyFile s t
            FSActionType_DeleteFile -> removeFile t
            FSActionType_SkipFile   -> return ()
            FSActionType_AddDir     -> createDirectory t
            FSActionType_DeleteDir  -> removeDirectoryRecursive t
            FSActionType_EnterDir   -> return ()
            FSActionType_LeaveDir   -> return ()

fsDo :: (FilePath, FilePath) -> [FSAction] -> IO ()
fsDo base actions = sequence [putFSAction a >> performFSAction base a | a <- actions] >> return ()


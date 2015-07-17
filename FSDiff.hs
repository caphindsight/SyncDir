module FSDiff (fsDiff) where

import MiscUtils
import FSInfo
import FSAction
import qualified Data.Map as M
import System.FilePath ((</>))

fsDiff :: FSInfo -> FSInfo -> [FSAction]
fsDiff = fsDiffRec ""

fsAddRec :: FilePath -> (String, FSInfo) -> [FSAction]
fsAddRec path (name, FSInfo_File _) = [FSAction FSActionType_AddFile (path </> name)]
fsAddRec path (name, FSInfo_Dir is) = [FSAction FSActionType_AddDir (path </> name)] ++
                                        (flatten $ map (fsAddRec (path </> name)) is)

fsDeleteRec :: FilePath -> (String, FSInfo) -> [FSAction]
fsDeleteRec path (name, FSInfo_File _) = [FSAction FSActionType_DeleteFile (path </> name)]
fsDeleteRec path (name, FSInfo_Dir  _) = [FSAction FSActionType_DeleteDir  (path </> name)]


fsMergeRec :: FilePath -> (String, FSInfo, FSInfo) -> [FSAction]
fsMergeRec path (name, FSInfo_File a, FSInfo_File b)
  | a > b      = [FSAction FSActionType_ModifyFile (path </> name)]
  | otherwise  = [FSAction FSActionType_SkipFile (path </> name)]
fsMergeRec path (name, a@(FSInfo_File _), b@(FSInfo_Dir _))  =
    fsDeleteRec path (name, a) ++ fsAddRec path (name, b)
fsMergeRec path (name, a@(FSInfo_Dir _) , b@(FSInfo_File _)) =
    fsDeleteRec path (name, a) ++ fsAddRec path (name, b)
fsMergeRec path (name, a@(FSInfo_Dir _), b@(FSInfo_Dir _)) =
    [FSAction FSActionType_EnterDir (path </> name)] ++
    fsDiffRec (path </> name) a b ++
    [FSAction FSActionType_LeaveDir (path </> name)]


fsDiffRec :: FilePath -> FSInfo -> FSInfo -> [FSAction]
fsDiffRec path (FSInfo_Dir source) (FSInfo_Dir target) =
    let s = M.fromList source;
        t = M.fromList target;
        new = M.toList $ M.difference s t;
        old = M.toList $ M.difference t s;
        reg = [(name, sourceInfo, targetInfo) | name <- M.keys (M.intersection s t),
                                                sourceInfo <- [s M.! name],
                                                targetInfo <- [t M.! name] ];

     in flatten (map (fsAddRec path) new) ++ flatten (map (fsMergeRec path) reg) ++ flatten (map (fsDeleteRec path) old)



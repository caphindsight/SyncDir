module FSTime (
    FSTime,
    fsTime
) where

import Data.Time.Clock (UTCTime)
import System.Directory (getModificationTime)

newtype FSTime = FSTime UTCTime deriving (Show, Eq, Ord)

fsTime :: FilePath -> IO FSTime
fsTime p = getModificationTime p >>= return . FSTime


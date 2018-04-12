{-# LANGUAGE RecordWildCards #-}
module LockedFile 
  ( LockedFile
  , mkLockedFile
  , lockedFileAppend
  ) where

import Control.Concurrent.Lock (Lock)
import qualified Control.Concurrent.Lock as Lock

data LockedFile = LockedFile
  { lfLock :: Lock
  , lfFileName :: FilePath
  }

mkLockedFile :: FilePath -> IO LockedFile
mkLockedFile lfFileName = do
  lfLock <- Lock.new
  return LockedFile{..}

lockedFileAppend :: LockedFile -> String -> IO ()
lockedFileAppend LockedFile{..} what = Lock.with lfLock $ 
  appendFile lfFileName what

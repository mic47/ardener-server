{-# LANGUAGE RecordWildCards #-}
module Lib
    ( ardenerServer
    ) where

import Data.Functor
import Data.Maybe
import Network
import System.IO  
import Control.Concurrent
import Control.Monad.Trans.RWS.Strict
import Control.Monad.IO.Class

import LockedFile
import Types

-- TODO (some random notes, not real plan): 
-- 1. Listen on port, and echo actions, and parse them.
-- 2. Periodically send some commands.
-- 3. Store stuff somewhere
--   What is good data source?
--   1. File cvs->Not structured, but easy to append, easy to use in other languages, easy to migrate
--   4. File jsonlines -> some structures, but not really. This is not really interesting
--   2. MySQL -> Structured, but easy to accidentally delete, hard to change structure
--   3. SQLite ->
-- 4. Proper data types for actions, parse messages into data types
-- 5. Store data types.
-- 6. Make acknowledge messages.
-- 7. Add administrative interface -- ability to connect to server, and send commands to other ardeners.
-- 8. Authentication.
-- 9. Proper command line options
-- 10. Introduce thrift?

-- Desing:
-- 1. Thread to accepting connections.
-- 2. For each ardener, have separate threads. This thread manages ardener state, and do all the logging.
-- 3. 

data ArdenerEnv = ArdenerEnv
  { dataFile :: LockedFile
  , actionFile :: LockedFile
  , ardenerHostname :: HostName -- TODO: actually log this
  , ardenerPort :: PortNumber -- TODO: actually log this
  }

data ArdenerState = ArdenerState
  { ardenerName :: Maybe String
  } deriving (Show, Read)

ardenerServer :: IO ()
ardenerServer = do
  let port = 2347
  sock <- listenOn $ PortNumber port
  putStrLn $ "Starting listening on port " ++ show port
  dataFile' <- mkLockedFile "dataFile.json"
  actionFile' <- mkLockedFile "actionFile.json"
  serverLoop (dataFile', actionFile') sock

serverLoop :: (LockedFile, LockedFile) -> Socket -> IO ()
serverLoop files@(dataFile, actionFile) socket = do
  (handle, ardenerHostname, ardenerPort) <- accept socket
  hSetBuffering handle NoBuffering -- Maybe LineBuffering?
  void $ forkIO $ void $ runRWST
    (handleArdener handle)
    (ArdenerEnv {..})
    (ArdenerState Nothing)
  serverLoop files socket

type ArIO a = RWST ArdenerEnv () ArdenerState IO a

handleArdener :: Handle -> ArIO ()
handleArdener handle = do
  line <- liftIO $ hGetLine handle
  case (decodeMessage line :: Either String ClientMessage) of
    Left err -> liftIO $ putStrLn $ "Error decoding of message '" ++ line ++ "': " ++ err
    Right msg -> do
      liftIO $ putStrLn $ show msg
      handleMessage msg
  handleArdener handle

handleMessage :: ClientMessage -> ArIO () 
handleMessage (IdentifySelf newName) = modify $ 
  \s -> s { ardenerName = Just newName }
handleMessage (MeasurementLog mLog) = do
  dataFile' <- dataFile <$> ask
  nm <- ardenerName <$> get 
  liftIO $ lockedFileAppend dataFile' 
    $ fromMaybe "" nm ++ "," ++ show mLog
handleMessage (ActionLog aLog) = do
  actionFile' <- actionFile <$> ask
  nm <- ardenerName <$> get
  liftIO $ lockedFileAppend actionFile'
    $ fromMaybe "" nm ++ "," ++ show aLog
handleMessage (ConfigDump _conf) = return ()
handleMessage (ConfigUpdated _conf) = return ()

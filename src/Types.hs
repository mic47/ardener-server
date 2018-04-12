{-# LANGUAGE RecordWildCards #-}
module Types
  ( MessageParser(..)
  , ArdenerAction(..)
  , ArdenerMeasurement(..)
  , ClientMessage(..)
  , ServerMessage(..)
  ) where

import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad.Trans.RWS.Strict
import Control.Monad.IO.Class
import Text.Read.Extra (readEither)

data ArdenerAction 
  = ManualWatering Int
  | AutomatedWatering Int
  deriving (Show, Read)

data ArdenerMeasurement = ArdenerMeasurement
  { moistureLevel :: Maybe Int
  , temperature :: Maybe Int
  , lightIntensity :: Maybe Int
  } 
  deriving (Show, Read)

data ClientMessage 
  = IdentifySelf String
  | MeasurementLog ArdenerMeasurement
  | ActionLog ArdenerAction
  | ConfigDump String
  | ConfigUpdated String
  deriving (Show)

data ServerMessage
  = DoMeasurement
  | GetConfigDump
  | SetConfig String
  | DoWatering Int
  deriving (Show)

class MessageParser a where
  decodeMessage :: String -> Either String a
  encodeMessage :: a -> String

instance MessageParser ArdenerAction where
  encodeMessage = show
  decodeMessage = readEither

instance MessageParser ArdenerMeasurement where
  encodeMessage ArdenerMeasurement{..} = mconcat $ intersperse "," $ catMaybes
    [ ("m" ++) . show <$> moistureLevel
    , ("t" ++) . show <$> temperature
    , ("l" ++) . show <$> lightIntensity
    ] 
  decodeMessage msg = foldl
    (\msr token -> do
      partialMeasurement <- msr
      case dropWhile (== ' ') token of
        'm':moisture -> do
           moisture' <- readEither moisture
           return partialMeasurement {moistureLevel = Just moisture'}
        't':temp -> do
           temperature' <- readEither temp
           return partialMeasurement {temperature = Just temperature'}
        'l':light -> do
           light' <- readEither light
           return partialMeasurement {lightIntensity = Just light'}
        _ -> return partialMeasurement
    )
    (Right $ ArdenerMeasurement Nothing Nothing Nothing)
    $ splitOn "," msg
    

instance MessageParser String where
  decodeMessage = return
  encodeMessage = id

instance MessageParser Int where
  decodeMessage = readEither 
  encodeMessage = show

instance MessageParser ClientMessage where
  decodeMessage x = case head' of
    "i" -> IdentifySelf <$> decodeMessage tail'
    "m" -> MeasurementLog <$> decodeMessage tail'
    "a" -> ActionLog <$> decodeMessage tail'
    "c" -> ConfigDump <$> decodeMessage tail'
    "cu" -> ConfigUpdated <$> decodeMessage tail'
    _ -> Left "Unknown command"
    where
      head' = takeWhile (/= ' ') x
      tail' = drop 1 $ dropWhile (/= ' ') x
  encodeMessage (IdentifySelf x) = "i " ++ encodeMessage x
  encodeMessage (MeasurementLog x) = "m " ++ encodeMessage x
  encodeMessage (ActionLog x) = "a " ++ encodeMessage x
  encodeMessage (ConfigDump x) = "c " ++ encodeMessage x
  encodeMessage (ConfigUpdated x) = "cu " ++ encodeMessage x

instance MessageParser ServerMessage where
  decodeMessage x = case head' of
    "m" -> Right DoMeasurement
    "gc" -> Right GetConfigDump
    "sc" -> SetConfig <$> decodeMessage tail'
    "dw" -> DoWatering <$> decodeMessage tail'
    _ -> Left "Unknown command"
    where
      head' = takeWhile (/= ' ') x
      tail' = drop 1 $ dropWhile (/= ' ') x
  encodeMessage DoMeasurement = "m"
  encodeMessage GetConfigDump = "gc"
  encodeMessage (SetConfig x) = "sc " ++ encodeMessage x
  encodeMessage (DoWatering x) = "dw " ++ encodeMessage x 

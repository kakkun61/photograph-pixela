{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Config where

import GHC.Generics
import qualified Web.Pixela as Pixela
import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Aeson
import Data.Time
import Data.Scientific (floatingOrInteger)
import Data.Default.Class

data Config =
  Config
    { timeZone :: TimeZone
    , pixelaGraphId :: Pixela.GraphId
    , pixela :: Pixela.Config
    }

data FileConfig =
  FileConfig
    { fileTimeZone :: Maybe TimeZone
    , filePixela :: PixelaConfig
    }
  deriving (Eq, Generic)

instance Yaml.FromJSON FileConfig where
  parseJSON =
    Aeson.genericParseJSON
      Aeson.defaultOptions
        { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop (length "file") }

data PixelaConfig =
  PixelaConfig
    { userName :: Pixela.UserName
    , token :: Pixela.Token
    , graphId :: Pixela.GraphId
    }
  deriving (Eq, Generic)

instance Yaml.FromJSON PixelaConfig where
  parseJSON =
    Aeson.genericParseJSON
      Aeson.defaultOptions
        { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' }

instance Yaml.FromJSON TimeZone where
  parseJSON =
    Yaml.withScientific "TimeZone" $ \n ->
      case floatingOrInteger n :: Either Double Int of
        Left _ -> fail "time zone must be an integer"
        Right z -> pure $ TimeZone z False ""

readConfig :: FilePath -> IO Config
readConfig path = do
  c <- Yaml.decodeFileThrow path
  tz <-
    case fileTimeZone c of
      Just tz' -> pure tz'
      Nothing -> getCurrentTimeZone
  let
    pfc = filePixela c
  pure $
    Config
      tz
      (graphId pfc)
      ( def
          { Pixela.userName = userName pfc
          , Pixela.token = token pfc
          }
      )

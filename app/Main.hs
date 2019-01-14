{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception       hiding (Exception)
import qualified Control.Exception       as E
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson.Lens
import qualified Data.ByteString         as ByteString
import           Data.Digest.Pure.MD5
import           Data.Foldable
import           Data.Maybe
import qualified Data.Serialize          as Cereal
import qualified Data.Text               as Text
import           Data.Time
import           Data.Version            (showVersion)
import           Graphics.HsExif
import           Options.Declarative
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Read               (readMaybe)
import           Type.Reflection
import qualified Web.Pixela              as Pixela

import           Paths_photograph_pixela (version)

import           Config                  (Config (Config, pixela), readConfig)

main :: IO ()
main =
  run "photograph-pixela" (Just $ showVersion version) command

command
  :: Flag "c" '["config"] "FILE" "configuration file. default \".photograph-pixela.yaml\"" (Def ".photograph-pixela.yaml" String)
  -> Flag "s" '["cache"] "DIR" "cache directory. default \".photograph-pixela\"" (Def ".photograph-pixela" String)
  -> Cmd "Upload shot dates to Pixela" ()
command configPath cachePath =
  liftIO $ do
    config@Config { pixela = pixelaConfig } <- readConfig $ get configPath
    pixelaClient <- Pixela.newClient pixelaConfig
    doDirectory config pixelaClient (get cachePath) "."

doDirectory :: Config -> Pixela.Client -> FilePath -> FilePath -> IO ()
doDirectory config pixelaClient cacheDir dir = do
  paths <- listDirectory dir
  (allDirs, files) <- dirFile dir paths
  let
    photos = filter ((`elem` [".ARW", ".JPG"]) . takeExtension) files
    dirs = filter (/= cacheDir) allDirs
  traverse_ (doFile config pixelaClient cacheDir dir) photos
  traverse_ (doDirectory config pixelaClient cacheDir . (dir </>)) dirs

doFile :: Config -> Pixela.Client -> FilePath -> FilePath -> FilePath -> IO ()
doFile (Config timeZone pixelaGraphId _) pixelaClient cacheDir dir filePath =
  do
    let
      cache = cacheDir </> dir </> filePath
      photo = dir </> filePath
    putStrLn $ dir </> filePath
    originalTime <- zonedTimeToUTC . (`ZonedTime` timeZone) <$> exifDateTimeOriginal photo
    let
      originalDay = utctDay originalTime
    cached <- doesFileExist cache
    if cached
      then do
        putStrLn "  cached"
        cachedTime <- getModificationTime cache
        putStrLn $ "  cached time:             " <> show cachedTime
        fileTime <- getModificationTime photo
        putStrLn $ "  file time:               " <> show fileTime
        putStrLn $ "  EXIF Date/Time Original: " <> show originalTime
        if fileTime <= cachedTime
          then putStrLn "  nothing to do"
          else do
            -- modified after the previous check
            -- compare file hashes
            putStrLn "modified after the previous check"
            hashValue <- hash' <$> ByteString.readFile (dir </> filePath) :: IO MD5Digest
            prevHashValueOrErro <- Cereal.decode <$> ByteString.readFile cache
            case prevHashValueOrErro of
              Right prevHashValue ->
                if hashValue == prevHashValue
                  then do
                    putStrLn "  update cached modification time"
                    setModificationTime cache fileTime
                  else do
                    updatePixela pixelaGraphId originalDay pixelaClient
                    putStrLn "  uploaded"
                    ByteString.writeFile cache $ Cereal.encode hashValue
              Left err -> throw $ Exception err
      else do
        putStrLn "  not cached"
        putStrLn $ "  EXIF Date/Time Original: " <> show originalTime
        hashValue <- hash' <$> ByteString.readFile (dir </> filePath) :: IO MD5Digest
        updatePixela pixelaGraphId originalDay pixelaClient
        putStrLn "  uploaded"
        createDirectoryIfMissing True $ cacheDir </> dir
        ByteString.writeFile cache $ Cereal.encode hashValue
  `catch` \e -> do
    hPutStrLn stderr $ displayException (e :: Exception)
    logFile $ "[" <> dir </> filePath <> "] " <> displayException e

dirFile :: FilePath -> [FilePath] -> IO ([FilePath], [FilePath])
dirFile dir = go ([], [])
  where
    go acc [] = pure acc
    go acc@(ds, fs) (path:rest) = do
      dp <- doesDirectoryExist $ dir </> path
      if dp
        then go (path:ds, fs) rest
        else do
          fp <- doesFileExist $ dir </> path
          if fp
            then go (ds, path:fs) rest
            else go acc rest

exifDateTimeOriginal :: FilePath -> IO LocalTime
exifDateTimeOriginal path = do
  exif <- either (throw . Exception) id <$> parseFileExif path
  maybe (throw $ Exception "no Date/Time Original") pure $ getDateTimeOriginal exif

updatePixela :: Pixela.GraphName -> Day -> Pixela.Client -> IO ()
updatePixela graphName day client = do
  mq <- (((>>= readMaybe . Text.unpack) . (^? key "quantity" . _String)) <$> Pixela.getQuantity graphName day client) `catch` ((\_ -> pure $ Just 0) :: SomeException -> IO (Maybe Int))
  let
    q = fromMaybe (throw $ Exception "can't parse as integer") mq
  Pixela.updateQuantity graphName day (show $ q + 1) client

logFile :: String -> IO ()
logFile message = do
  tz <- getCurrentTimeZone
  utct <- getCurrentTime
  let
    time = utcToLocalTime tz utct
    timeString = formatTime defaultTimeLocale "%F %T" time
  appendFile ".photograph-pixela.log" $ "[" <> timeString <> "] " <> message <> "\n"

newtype Exception = Exception String deriving (Show, Typeable)

instance E.Exception Exception where
  displayException (Exception message) = message

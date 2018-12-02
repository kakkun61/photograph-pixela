{-# LANGUAGE DataKinds #-}

module Main where

import qualified Data.ByteString as ByteString
import Data.Digest.Pure.MD5
import Data.Foldable
import Data.Time
import Graphics.HsExif
import System.FilePath
import System.Directory
import qualified Data.Serialize as Cereal
import qualified Web.Pixela as Pixela
import qualified Data.Yaml as Yaml
import Options.Declarative
import Data.Version (showVersion)
import Control.Monad.IO.Class

import Paths_photograph_pixela (version)

import Config (Config (Config), readConfig)

main :: IO ()
main =
  run "photograph-pixela" (Just $ showVersion version) command

command
  :: Flag "c" '["config"] "FILE" "configuration file. default \".photography-pixela.yaml\"" (Def ".photography-pixela.yaml" String)
  -> Flag "s" '["cache"] "DIR" "cache directory. default \".photography-pixela\"" (Def ".photography-pixela" String)
  -> Cmd "Upload shot dates to Pixela" ()
command configPath cachePath =
  liftIO $ do
    config <- readConfig $ get configPath
    doDirectory config (get cachePath) "."

doDirectory :: Config -> FilePath -> FilePath -> IO ()
doDirectory config cacheDir dir = do
  paths <- listDirectory dir
  (dirs, files) <- dirFile dir paths
  let
    photos = filter ((`elem` [".ARW", ".JPG"]) . takeExtension) files
  traverse_ (doFile config cacheDir dir) photos
  traverse_ (doDirectory config cacheDir) dirs

doFile :: Config -> FilePath -> FilePath -> FilePath -> IO ()
doFile (Config timeZone _ _) cacheDir dir filePath = do
  let
    cache = cacheDir </> dir </> filePath
    photo = dir </> filePath
  cached <- doesFileExist $ cache
  if cached
    then do
      putStrLn $ "cached: " <> dir </> filePath
      cachedTime <- getModificationTime cache
      print cachedTime
      fileTime <- getModificationTime photo
      print fileTime
      originalTime <- zonedTimeToUTC . (\lt -> ZonedTime lt timeZone) <$> exifDateTimeOriginal photo
      print originalTime
      let originalDay = utctDay originalTime
      if fileTime <= cachedTime
        then putStrLn "nothing to do"
        else do
          -- modified after the previous check
          -- compare file hashes
          putStrLn "modified after the previous check"
          hashValue <- hash' <$> ByteString.readFile (dir </> filePath) :: IO MD5Digest
          print hashValue
          prevHashValueOrErro <- Cereal.decode <$> ByteString.readFile cache
          case prevHashValueOrErro of
            Right prevHashValue -> do
              print prevHashValue
              if hashValue == prevHashValue
                then do
                  putStrLn "update cached modificationtime"
                  setModificationTime cache fileTime
                else do
                  putStrLn "TODO: upload"
                  ByteString.writeFile cache $ Cereal.encode hashValue
            Left err -> error err
    else do
      putStrLn $ "not cached: " <> dir </> filePath
      hashValue <- hash' <$> ByteString.readFile (dir </> filePath) :: IO MD5Digest
      print hashValue
      putStrLn "TODO: upload"
      createDirectoryIfMissing True $ cacheDir </> dir
      ByteString.writeFile cache $ Cereal.encode hashValue

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
  exif <- either error id <$> parseFileExif path
  maybe (error "no Data/Time Original") pure $ getDateTimeOriginal exif

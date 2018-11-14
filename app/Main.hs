module Main where

import qualified Data.ByteString as ByteString
import Data.Digest.Pure.MD5
import Data.Foldable
import Data.Time
import Graphics.HsExif
import System.FilePath
import System.Directory
import Data.Serialize

main :: IO ()
main = do
  timeZone <- getCurrentTimeZone
  doFile timeZone ".photography-pixela" "." "_DSC1098.ARW"

doDirectory :: TimeZone -> FilePath -> FilePath -> IO ()
doDirectory timeZone cacheDir dir = do
  paths <- listDirectory dir
  (dirs, files) <- dirFile dir paths
  let
    photos = filter ((`elem` [".ARW", ".JPG"]) . takeExtension) files
  for_ photos $ doFile timeZone cacheDir dir

doFile :: TimeZone -> FilePath -> FilePath -> FilePath -> IO ()
doFile timeZone cacheDir dir filePath = do
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
      originalTime <- zonedTimeToUTC  . (\lt -> ZonedTime lt timeZone) <$> exifDateTimeOriginal photo
      print originalTime
      if fileTime <= cachedTime
        then putStrLn "nothing to do"
        else do
          -- modified after the previous check
          -- compare file hashes
          putStrLn "modified after the previous check"
          hashValue <- hash' <$> ByteString.readFile (dir </> filePath) :: IO MD5Digest
          print hashValue
          prevHashValueOrErro <- decode <$> ByteString.readFile cache
          case prevHashValueOrErro of
            Right prevHashValue -> do
              print prevHashValue
              if hashValue == prevHashValue
                then do
                  putStrLn "update cached modificationtime"
                  setModificationTime cache fileTime
                else do
                  putStrLn "TODO: upload"
                  ByteString.writeFile cache $ encode hashValue
            Left err -> error err
    else do
      putStrLn $ "not cached: " <> dir </> filePath
      hashValue <- hash' <$> ByteString.readFile (dir </> filePath) :: IO MD5Digest
      print hashValue
      putStrLn "TODO: upload"
      createDirectoryIfMissing True $ cacheDir </> dir
      ByteString.writeFile cache $ encode hashValue

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

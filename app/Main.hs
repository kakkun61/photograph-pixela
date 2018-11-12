module Main where

import qualified Data.ByteString as ByteString
import Data.Digest.Pure.MD5
import Data.Foldable
import Data.Time
import System.Process
import System.FilePath
import System.Directory

main :: IO ()
main = do
  doFile ".photography-pixela" "." "_DSC1103.ARW"

doDirectory :: FilePath -> FilePath -> IO ()
doDirectory cacheDir dir = do
  paths <- listDirectory dir
  (dirs, files) <- dirFile dir paths
  let
    photos = filter ((`elem` [".ARW", ".JPG"]) . takeExtension) files
  for_ photos $ doFile cacheDir dir

doFile :: FilePath -> FilePath -> FilePath -> IO ()
doFile cacheDir dir filePath = do
  let
    cache = cacheDir </> dir </> filePath
  cached <- doesFileExist $ cache
  if cached
    then do
      putStrLn $ "cached: " <> dir </> filePath
      cachedTime <- getModificationTime cache
      print cachedTime
    else do
      putStrLn $ "not cached: " <> dir </> filePath
      md5 <- hash' <$> ByteString.readFile (dir </> filePath) :: IO MD5Digest
      print md5
      createDirectoryIfMissing True $ cacheDir </> dir
      writeFile cache $ show md5

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

exifDateTimeOriginal = do
  dt <-
    readProcess
      "exiftool"
      ["-DateTimeOriginal", path]
      ""
      >>=
        parseTimeM
          True
          defaultTimeLocale
          "%Y:%m:%d %T"

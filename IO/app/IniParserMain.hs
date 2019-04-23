module IniParserMain where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import qualified Data.Map as M
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Text.Trifecta

import IniParser


parseIniFile :: MonadIO m
             => FilePath
             -> m (FilePath, Maybe Config)
parseIniFile path =
    ((,) path) <$> parseFromFile parseIni path


getIniFiles :: FilePath -> IO [FilePath]
getIniFiles path =
    listDirectory path
     >>= return . filter (isSuffixOf ".ini")
     >>= return . map ((</>) path)
     >>= filterM doesFileExist


getDir :: IO FilePath
getDir = do
    args <- getArgs
    path <-
        case args of
            []       -> die "No path specified."
            (path:_) -> return path
    dirExists <- doesPathExist path
    case dirExists of
        True -> return path
        False -> die "Invalid path."


main :: IO ()
main = do
    result <- 
        getDir 
         >>= getIniFiles
         >>= mapM parseIniFile
         >>= return . M.fromList
    putStrLn $ show result
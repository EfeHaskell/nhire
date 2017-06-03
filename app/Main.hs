module Main where

import Order

import System.Directory (getCurrentDirectory, doesFileExist)
import System.Environment (getArgs)
import qualified Data.List as List
import Control.Monad (liftM, when, unless)

import System.IO (openFile, FilePath, Handle, hClose, hGetLine, hIsEOF, IOMode(ReadMode))

import Pipes
import qualified Pipes.Prelude as P

import Paths_nhire (getDataDir)



replayCsv :: FilePath -> Producer Order IO ()
replayCsv filePath = do
  fileHandle <- liftIO $ openFile filePath ReadMode
  pumpOrder fileHandle
  lift $ hClose fileHandle

  where 
    pumpOrder fileHandle = do
      eof <- lift $ hIsEOF fileHandle
      unless eof $ do
        aCsvLine <- lift $ hGetLine fileHandle
        yield $ getOrderFromLine aCsvLine

        pumpOrder fileHandle

main :: IO ()
main = do
  parentDir <- (flip (++) "/") <$> getDataDir
  fileNames <- getArgs
  mapM_ putStrLn fileNames

  when ((List.length fileNames) == 3) $ do
    putStrLn "Three input csv file names specified!\n"

    let fileNamesFullPaths = List.map ((++) parentDir) fileNames

    csvFilesExistences <- mapM doesFileExist fileNamesFullPaths
    
    when (List.all ((==) True) csvFilesExistences) $ do
      putStrLn "All input csv files exist. Processing will begin!\n"

      let firstFullPath = fileNamesFullPaths !! 0
      runEffect $ for (replayCsv firstFullPath) $ (lift . print)
  
  putStrLn "End"

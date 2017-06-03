module Main where

import Order

import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import qualified Data.List as List
import Control.Monad (liftM)
import System.IO (openFile, FilePath, Handle, hClose, hGetLine, hIsEOF, IOMode(ReadMode))
import Control.Monad (unless)

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
  fileNames <- getArgs
  mapM_ putStrLn fileNames

  parentDir <- (flip (++) "/") <$> getDataDir

  let firstFileName = fileNames !! 0

  runEffect $ for (replayCsv (parentDir ++ firstFileName)) $ (lift . print)
  
  putStrLn "End"

-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE FlexibleContexts #-}  
module Main where

import Control.Concurrent hiding (yield)
import Control.Concurrent.Chan
import Control.Monad (liftM, when, unless)

import qualified Data.List as List
import Data.IORef

import Pipes
import qualified Pipes.Prelude as P

import System.Directory (getCurrentDirectory, doesFileExist)
import System.Environment (getArgs)

import System.IO (openFile, FilePath, Handle, hClose, hGetLine, hIsEOF, IOMode(ReadMode))
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Paths_nhire (getDataDir)

import Order



replayCsv :: FilePath -> Producer Order IO ()
replayCsv filePath = do
  fileHandle <- liftIO $ openFile filePath ReadMode
  programStartUtcTime <- liftIO getCurrentTime

  pumpOrder fileHandle programStartUtcTime
  lift $ hClose fileHandle

  where 
    pumpOrder :: Handle -> UTCTime -> Proxy x' x () Order IO ()
    pumpOrder fileHandle programStart = do
      eof <- lift $ hIsEOF fileHandle
      unless eof $ do
        aCsvLine <- lift $ hGetLine fileHandle
        let order = getOrderFromLine aCsvLine
        let orderTime = realToFrac $ (timestamp order * 1000) :: Double

        currentUtcTime <- lift getCurrentTime
        let timeElapsedSoFar = (realToFrac $ diffUTCTime currentUtcTime programStart :: Double)
        let timeToWait = orderTime - timeElapsedSoFar
        lift $ putStrLn $ "Time to wait: " ++ (show timeToWait) ++ " seconds"

        lift $ threadDelay (round (timeToWait / 1000000)) -- microseconds
        yield order

        pumpOrder fileHandle programStart


-- Prints to std out every 1s
aggregateAndLog :: [Producer Order IO ()] -> IO ()
aggregateAndLog orderProducers = do
  runningThreadsCounter <- newChan
  writeChan runningThreadsCounter $ length orderProducers -- Three processing threads currently running

  -- let aggregate = replicate (length orderProducers) 

  forkIO $ do 
    runEffect $ for (orderProducers !! 0) (lift . print) 
    writeChan runningThreadsCounter 0

  keepMainProgramAlive runningThreadsCounter
  

keepMainProgramAlive :: Chan Int -> IO ()
keepMainProgramAlive keepAlive = do
  numThreadsAlive <- readChan keepAlive

  when (numThreadsAlive == 0) $ do
    putStrLn "Processing threads have ended. Will end main thread now"
    return ()
  keepMainProgramAlive keepAlive


main :: IO ()
main = do
  parentDir <- getDataDir
  fileNames <- getArgs

  when ((List.length fileNames) == 3) $ do
    putStrLn "Three input csv file names specified!\n"

    let fileNamesFullPaths = List.map ((++) ((++) parentDir "/")) fileNames

    csvFilesExistences <- mapM doesFileExist fileNamesFullPaths
    
    when (List.all ((==) True) csvFilesExistences) $ do
      putStrLn "All input csv files exist. Processing will begin!\n"
      aggregateAndLog $ map replayCsv fileNamesFullPaths
  

--------------------------------------------------------------------------------
--
--  Copyright (c) 2012 - 2014 Tad Doxsee
--  All rights reserved.
--
--  Author: Tad Doxsee
--
--------------------------------------------------------------------------------

{-# LANGUAGE DoAndIfThenElse #-}

module Main where

-- base
import System.Environment (getArgs)
-- import Control.Monad (liftM)

-- filepath
import System.FilePath ((</>))
import System.IO

-- directory
import System.Directory (doesFileExist)

-- HSH
import HSH.Command (run, runIO)

main :: IO ()
main = do
  
  args <- getArgs :: IO [String]

  let nArgs   = length args
      okNArgs = nArgs == 4

  if not okNArgs
    then do
       putStrLn
         "usage: gui-diff gui-differ testListFile stdsDir testsDir"
    else do
      
      let guiDiffer   = args !! 0 :: String -- ^ name of differ in path, e.g. meld, KDiff3
          testListFile = args !! 1 :: FilePath
          stdsDir      = args !! 2 :: FilePath
          testsDir     = args !! 3 :: FilePath
          
      testListFileExists <- doesFileExist testListFile

      case testListFileExists of
        False -> putStrLn $ "update-stds: test list file " ++ testListFile
                             ++ " does not exist"
        _     -> do


          h <- openFile testListFile ReadMode :: IO Handle
          s <- hGetContents h                 :: IO String
          let testNames = lines s             :: [String]

          mapM_ (runDiffer guiDiffer stdsDir testsDir) testNames

runDiffer :: String -> FilePath -> FilePath -> String -> IO ()

runDiffer guiDiffer stdsDirName testsDirName testName = do
  
  let
    stdDir      = stdsDirName  </> testName
    stdOutDir   = stdDir       </> "o"
--    stdOut      = stdOutDir    </> testName ++ "_out.txt"
    stdOut      = stdOutDir    </> "out.txt"

    testDir     = testsDirName </> testName
    testOutDir  = testDir      </> "o"
--    testOut     = testOutDir   </> testName ++ "_out.txt"
    testOut     = testOutDir   </> "out.txt"
    
    diffFile    = testDir      </> "diff"

  stdOutExists   <- doesFileExist stdOut
  testOutExists  <- doesFileExist testOut
  isDiff         <- fileContainsData diffFile
  
  if (not stdOutExists)
  then error ("std out file " ++ stdOut ++ " does not exist")
  else do
    if (not testOutExists)
    then error ("test out file " ++ testOut ++ " does not exist")
    else do
      putStrLn testName
      if isDiff
      then runIO $ guiDiffer ++ " " ++ stdOut ++ " " ++ testOut
      else putStrLn "no diff"

fileContainsData :: FilePath -> IO Bool
fileContainsData fp = do
  wcStr <- run $ "wc " ++ fp :: IO String
  let nLines = read $ head $ words wcStr
  return $ not $ nLines == 0
  

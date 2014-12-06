--------------------------------------------------------------------------------
--
--  Copyright (c) 2011 - 2014 Tad Doxsee
--  All rights reserved.
--
--  Author: Tad Doxsee
--
--------------------------------------------------------------------------------

{-# LANGUAGE DoAndIfThenElse #-}

module Main where

-- base
import System.Environment (getArgs)

-- filepath
import System.FilePath ((</>))
import System.IO

-- directory
import System.Directory ( doesDirectoryExist
                        , doesFileExist
                        , createDirectory
                        , createDirectoryIfMissing
                        )

-- HSH
import HSH.Command (run, runIO, (-|-))

main :: IO ()
main = do
  
  args <- getArgs :: IO [String]

  let nArgs = length args
      okNArgs = nArgs == 3

  if not okNArgs
    then do
       putStrLn
         "usage: update-stds testListFile stdsDir testsDir"
    else do
      
      let testListFile = args !! 0 :: FilePath
          stdsDir      = args !! 1 :: FilePath
          testsDir     = args !! 2 :: FilePath
          
      testListFileExists <- doesFileExist testListFile

      case testListFileExists of
        False -> putStrLn $ "update-stds: test list file " ++ testListFile
                             ++ " does not exist"
        _     -> do


          h <- openFile testListFile ReadMode :: IO Handle
          s <- hGetContents h                 :: IO String
          let testNames = lines s             :: [String]

          mapM_ (updateStd stdsDir testsDir) testNames

updateStd :: FilePath -> FilePath -> String -> IO ()

updateStd stdsDirName testsDirName testName = do
  
  let
    stdDir      = stdsDirName  </> testName
    stdOutDir   = stdDir       </> "o"
    stdOut      = stdOutDir    </> "out.txt"
--    stdOut      = stdOutDir    </> testName ++ "_out.txt"

    testDir     = testsDirName </> testName
    testOutDir  = testDir      </> "o"
    testOut     = testOutDir   </> "out.txt"

  stdOutExists  <- doesFileExist stdOut
  testOutExists <- doesFileExist testOut
  
  if (not stdOutExists)
  then error ("std out file " ++ stdOut ++ " does not exist")
  else do
    if (not testOutExists)
    then error ("test out file " ++ testOut ++ " does not exist")
    else do
      putStrLn testName
      runIO $ "cp " ++ testOut ++ " " ++ stdOut


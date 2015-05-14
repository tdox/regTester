--------------------------------------------------------------------------------
--
--  Copyright (c) 2011 - 2014 Tad Doxsee
--  All rights reserved.
--
--  Author: Tad Doxsee
--
--------------------------------------------------------------------------------

{-# LANGUAGE DoAndIfThenElse #-}

module RegTesterLib where

-- base
import Data.Either        (lefts)
import Data.Maybe         (fromJust)
import System.Environment (getArgs)
import System.Exit        (ExitCode(ExitSuccess), exitSuccess)

-- filepath
import System.FilePath ((</>))
import System.IO

-- directory
import System.Directory ( doesDirectoryExist
                        , doesFileExist
                        -- , createDirectory
                        , createDirectoryIfMissing
                        )

-- HSH
import HSH.Command (run, runIO)

--------------------------------------------------------------------------------

type CmdMaker = ProgramName -> FilePath -> FilePath -> FilePath -> FilePath
                -> String

updateAllStds :: IO ()
updateAllStds = do
  
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


updateOneStd :: IO ()
updateOneStd = do
  
  args <- getArgs :: IO [String]

  let nArgs = length args
      okNArgs = nArgs == 3

  if not okNArgs
    then do
       putStrLn
         "usage: update-stds testName stdsDir testsDir"
    else do
      
      let testName = args !! 0 :: FilePath
          stdsDir  = args !! 1 :: FilePath
          testsDir = args !! 2 :: FilePath

      updateStd stdsDir testsDir testName
          
  

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

  -- stdOutExists  <- doesFileExist stdOut
  testOutExists <- doesFileExist testOut
  
  --if (not stdOutExists)
  --then error ("std out file " ++ stdOut ++ " does not exist")
  --else do
  if (not testOutExists)
  then error ("test out file " ++ testOut ++ " does not exist")
  else do
    putStrLn testName
    runIO $ "cp " ++ testOut ++ " " ++ stdOut




regTesterMain :: CmdMaker -> IO ()
regTesterMain mkCmd = do

  args <- getArgs :: IO [String]

  let nArgs   = length args
      okNArgs = nArgs == 6 -- || nArgs == 5

  if not okNArgs
    then do
       putStrLn $ "usage: regTester testListFile program differ "
                   ++ "stdsDir testsDir"
    else do

      let testListFile = args !! 0 :: FilePath
          program      = args !! 1 :: String
          differ       = args !! 2 :: String
          auxDir       = args !! 3 :: FilePath
          stdsDir      = args !! 4 :: FilePath
          testsDir     = args !! 5 :: FilePath
          
      testListFileExists <- doesFileExist testListFile

      case testListFileExists of
        False -> do
          putStrLn $ "regTester: test list file " ++ testListFile
                     ++ " does not exist"
          exitSuccess
                 
        _     -> do


          h <- openFile testListFile ReadMode :: IO Handle
          s <- hGetContents h                 :: IO String
          let testNames = lines s             :: [String]

          results <-
            runRegTests program differ auxDir stdsDir
                        (Just testsDir) testNames mkCmd
            
          -- mapM_ showResults (zip testNames results) -- *
          
          let result = if length (lefts results) > 0
                       then "Failure"
                       else "Pass"

          putStrLn result
          hFlush stdout


showResults :: (String, Either String Bool) -> IO ()
showResults (testName, result) = do
  putStr $ testName ++ ": "
  case result of
    Left errMsg -> putStrLn errMsg
    Right _     -> putStrLn "Pass"


type ProgramName = String

runRegTests :: ProgramName
            -> ProgramName
            -> FilePath
            -> FilePath
            -> Maybe FilePath
            -> [String]
            -> CmdMaker
            -> IO [Either String Bool]

runRegTests program differ auxDir stdsDirName mTestsDir testNames cmdMaker = do

  -- putStrLn "runRegTests"
  -- putStrLn program -- *

  let testsDir :: FilePath
      testsDir = case mTestsDir of
                 Nothing -> "."
                 _       -> fromJust mTestsDir

  stdsDirExists <- doesDirectoryExist stdsDirName

  case stdsDirExists of
    False -> do
      let errMsg = "runRegTest:  standards directory " ++ 
                   show stdsDirName ++ " does not exist."
                
      putStrLn errMsg
                            
      return [Left errMsg]
    _     -> do

      testsDirExists <- doesDirectoryExist testsDir

      case testsDirExists of
        {- False -> return [Left $ "runRegTest:  tests directory " ++ 
                                show testsDir ++ " does not exist."] -}

        _     -> mapM (runRegTest program differ auxDir
                                  stdsDirName testsDir cmdMaker)
                      testNames
  



runRegTest :: ProgramName
           -> ProgramName
           -> FilePath
           -> FilePath
           -> FilePath
           -> CmdMaker
           -> String
           -> IO (Either String Bool)

runRegTest program differ auxDir stdsDirName testsDir mkCmd testName = do

  -- putStrLn program -- *
  putStr $ testName ++ " "

  let
    stdDir      = stdsDirName </> testName
    stdInDir    = stdDir      </> "i"
    stdOutDir   = stdDir      </> "o"
    stdOutFile  = stdOutDir   </> "out.txt"
    testDir     = testsDir    </> testName
    testOutDir  = testDir     </> "o"
    testOutFile = testOutDir  </> "out.txt"

  testDirExists <- doesDirectoryExist testDir

  case testDirExists of
   {-
   True -> do
     putStrLn $ "runRegTest: test directory " ++ show testDir ++
                                " already exists."
     return $ Left $ "runRegTest: test directory " ++ show testDir ++
                                " already exists."
-}
   _    -> do


    createDirectoryIfMissing True testOutDir
    -- let cmd = program ++ " " ++ stdInDir ++ "/test.hs " ++  testOutFile


    let cmd = mkCmd program auxDir stdInDir testOutDir testOutFile
    -- putStrLn $ "cmd: " ++ cmd -- *
    runIO cmd
    
    (diffs, action) <- run $ (differ ++ " " ++ stdOutFile ++ " "
                                      ++ testOutFile
                                     )  :: IO (String, IO (String, ExitCode))
    --putStrLn $ "diffs: " ++ show diffs
                                             
    writeFile (testDir </> "diff") diffs
    (_, exitCode) <- action :: IO (String, ExitCode)
    --putStrLn $ show exitCode
    --writeFile (testDir </> "diff") diffs

    -- hFlush stdout
    case exitCode of
     ExitSuccess -> do
       putStrLn " Pass"
       return $ Right True
     _           -> do
       putStrLn " Fail"
       return $ Left "Diff failure"


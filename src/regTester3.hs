--------------------------------------------------------------------------------
--
--  Copyright (c) 2011 - 2014 Tad Doxsee
--  All rights reserved.
--
--  Author: Tad Doxsee
--
--------------------------------------------------------------------------------


module Main where

-- base
import Data.Either        (lefts)
import Data.Maybe         (fromJust)
import System.Environment (getArgs)
import System.IO          (Handle, IOMode(ReadMode), hFlush, hGetContents,
                           openFile, stdout)
import System.Exit        (ExitCode(ExitSuccess), exitSuccess)

-- filepath
import System.FilePath ((</>))

-- directory
import System.Directory ( doesDirectoryExist
                        , doesFileExist
                        , createDirectoryIfMissing
                        )

-- HSH
import HSH.Command (run, runIO)

main :: IO ()
main = do

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
          
{-          testsDir = case nArgs of
            4 -> Nothing
            5 -> Just $ args !! 4 -}

      --putStrLn $ "auxDir: " ++ auxDir

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
            runRegTests program differ auxDir stdsDir (Just testsDir) testNames
            
          --mapM_ showResults (zip testNames results)
          
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
            -> IO [Either String Bool]

runRegTests program differ auxDir stdsDirName mTestsDir testNames = do

  --putStrLn "runRegTests"

  let testsDir :: FilePath
      testsDir = case mTestsDir of
                 Nothing -> "."
                 _       -> fromJust mTestsDir

  stdsDirExists <- doesDirectoryExist stdsDirName

  case stdsDirExists of
    False -> return [Left $ "runRegTest:  standards directory " ++ 
                            show stdsDirName ++ " does not exist."]
    _     -> do

      testsDirExists <- doesDirectoryExist testsDir

      case testsDirExists of
        False -> return [Left $ "runRegTest:  tests directory " ++ 
                                show testsDir ++ " does not exist."]

        _     -> mapM (runRegTest program differ auxDir stdsDirName testsDir)
                      testNames
  



runRegTest :: ProgramName
           -> ProgramName
           -> FilePath
           -> FilePath
           -> FilePath
           -> String
           -> IO (Either String Bool)

runRegTest program differ auxDir stdsDirName testsDir testName = do

  putStr $ testName ++ " "

  let
    stdDir      = stdsDirName </> testName
    stdInDir    = stdDir      </> "i"
    stdOutDir   = stdDir      </> "o"
--    stdOut      = stdOutDir   </> testName ++ "_out.txt"
    stdOutFile  = stdOutDir   </> "out.txt"
--    inputFile   = stdInDir    </> testName ++ "_in.txt"
--    input       = "."         </> testName ++ "_in.txt"

    testDir     = testsDir    </> testName
    testOutDir  = testDir     </> "o"
--    output      = testOutDir  </> testName ++ "_out.txt"
    testOutFile = testOutDir  </> "out.txt"

--  inputExists <-doesFileExist inputFile

{-
  case inputExists of
    False -> return $ Left $ "runRegTest: input file " ++ 
                              show input ++ " does not exist."
    _     -> do
-}

  --putStrLn $ "auxDir: " ++ auxDir
  --putStrLn $ "stdsDirName: " ++ stdsDirName

  testDirExists <- doesDirectoryExist testDir

  case testDirExists of
   True -> do
     putStrLn $ "runRegTest: test directory " ++ show testDir ++
                                " already exists."
     return $ Left $ "runRegTest: test directory " ++ show testDir ++
                                " already exists."
   _    -> do

{-     
     stdOutExists <- doesFileExist stdOut
          
     case stdOutExists of
      False ->  return $ Left $ "runRegTest: standard output  " ++ 
                               show stdOut ++ " does not exist."
      _     -> do
-}

    createDirectoryIfMissing True testOutDir
              -- let cmd = "cp " ++ stdInDir ++ "/* ."
              -- runIO $ "cp " ++ stdInDir ++ "/* ."
              -- runIO $ program ++ " " ++ input ++ " " ++ output
    let cmd = program ++ " " ++ stdInDir ++ "/test.hs " ++  testOutFile
    --putStrLn $ "cmd: " ++ cmd
    runIO cmd
    -- runIO $ program ++ " " ++ stdInDir ++ "/test.hs > " ++  testOutFile
    --runIO $ program ++ " " ++ auxDir ++ " " ++ stdInDir ++ " " ++ testOutDir
              -- runIO $ "rm " ++ input

    --putStrLn "ran cmd"
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




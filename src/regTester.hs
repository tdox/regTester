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
import Data.Either        (rights)
import Data.Maybe         (fromJust)
import System.Environment (getArgs)
import System.IO       -- (openFile, IOMode, hGetContents)
import System.Exit     -- (ExitCode, ExitSuccess)

-- filepath
import System.FilePath ((</>))

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

  let nArgs   = length args
      okNArgs = nArgs == 4 || nArgs == 5

  if not okNArgs
    then do
       putStrLn
         "usage: regTester testListFile program differ stdsDir [testsDir]"
    else do

      let testListFile = args !! 0 :: FilePath
          program      = args !! 1 :: String
          differ       = args !! 2 :: String
          stdsDir      = args !! 3 :: FilePath
          
          testsDir = case nArgs of
            4 -> Nothing
            5 -> Just $ args !! 4

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

          results <- runRegTests program differ stdsDir testsDir testNames
          let rightRslts = rights results

          mapM_ showResults (zip testNames results)
          
          let result = if length rightRslts > 0 && and rightRslts
                       then "Pass"
                       else "Failure"

          putStrLn result


showResults :: (String, Either String Bool) -> IO ()
showResults (testName, result) = do
  putStr $ testName ++ ": "
  case result of
    Left errMsg -> putStrLn errMsg
    Right b     -> putStrLn "Pass"


type ProgramName = String

runRegTests :: ProgramName
            -> ProgramName
            -> FilePath
            -> Maybe FilePath
            -> [String]
            -> IO [Either String Bool]

runRegTests program differ stdsDirName testsDir' testNames = do

  let testsDir :: FilePath
      testsDir = case testsDir' of
                 Nothing -> "."
                 _       -> fromJust testsDir'

  stdsDirExists <- doesDirectoryExist stdsDirName

  case stdsDirExists of
    False -> return [Left $ "runRegTest:  standards directory " ++ 
                            show stdsDirName ++ " does not exist."]
    _     -> do

      testsDirExists <- doesDirectoryExist testsDir

      case testsDirExists of
        False -> return [Left $ "runRegTest:  tests directory " ++ 
                                show testsDir ++ " does not exist."]

        _     -> mapM (runRegTest program differ stdsDirName testsDir) testNames
  



runRegTest :: ProgramName
           -> ProgramName
           -> FilePath
           -> FilePath
           -> String
           -> IO (Either String Bool)

runRegTest program differ stdsDirName testsDir testName = do

--  putStrLn $ testName

  let
    stdDir      = stdsDirName </> testName
    stdInDir    = stdDir      </> "i"
    stdOutDir   = stdDir      </> "o"
    stdOut      = stdOutDir   </> testName ++ "_out.txt"
    inputFile   = stdInDir    </> testName ++ "_in.txt"
    input       = "."         </> testName ++ "_in.txt"

    testDir     = testsDir    </> testName
    testOutDir  = testDir     </> "o"
    output      = testOutDir  </> testName ++ "_out.txt"

  inputExists <-doesFileExist inputFile

  case inputExists of
    False -> return $ Left $ "runRegTest: input file " ++ 
                              show input ++ " does not exist."
    _     -> do

      testDirExists <- doesDirectoryExist testDir

      case testDirExists of
        True -> return $ Left $ "runRegTest: test directory " ++ show testDir ++
                                " already exists."
        _    -> do

          stdOutExists <- doesFileExist stdOut
          
          case stdOutExists of
            False ->  return $ Left $ "runRegTest: standard output  " ++ 
                                       show stdOut ++ " does not exist."
            _     -> do

              createDirectoryIfMissing True testOutDir
              let cmd = "cp " ++ stdInDir ++ "/* ."
              runIO $ "cp " ++ stdInDir ++ "/* ."
              runIO $ program ++ " " ++ input ++ " " ++ output
              runIO $ "rm " ++ input


              (diffs, action) <- run $ (differ ++ " " ++ stdOut ++ " "
                                               ++ output
                                       )  :: IO (String, IO (String, ExitCode))
                                             
              (_, exitCode) <- action :: IO (String, ExitCode)
--              putStrLn $ show exitCode
              writeFile (testDir </> "diff") diffs

              case exitCode of
                ExitSuccess -> return $ Right True
                _           -> return $ Left "Diff failure"




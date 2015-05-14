--------------------------------------------------------------------------------
--
--  Copyright (c) 2011 - 2014 Tad Doxsee
--  All rights reserved.
--
--  Author: Tad Doxsee
--
--------------------------------------------------------------------------------


module Main where
import RegTesterLib (CmdMaker, regTesterMain)

main :: IO ()
main = regTesterMain mkCmd


mkCmd :: CmdMaker
mkCmd program auxDir stdInDir testOutDir _ =
  program ++ " " ++ auxDir ++ " " ++ stdInDir ++ " " ++ testOutDir
